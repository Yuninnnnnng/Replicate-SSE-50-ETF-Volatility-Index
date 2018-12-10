#template for calculating 50etf ivx
#start wind
library(WindR)
w.start()
library(data.table)

#initiate system
StartDate = as.Date('11/01/2018',"%m/%d/%Y") 
EndDate =as.Date('11/26/2018',"%m/%d/%Y")
ACode = "510050.SH" #wind code of underlying asset: 50etf
CEndDate = as.Date('11/28/2018',"%m/%d/%Y") #last trading day of this month's contracts
CEndDate1 = as.Date('12/26/2018',"%m/%d/%Y")#last trading day of next month's contracts

#contracts'codes are loaded and stored in dt:code
code <- na.omit(code)
CCode <-as.character(code$c)
PCode <- as.character(code$p)
NumCon = length(CCode) # number of contracts call = put
Days = as.character(w.tdays(StartDate,EndDate)$Data$DATETIME)#get date sequence
K = as.numeric(code$k)
df <- cbind(CCode,K,PCode)
colnames(df) <- c('CCode','K','PCode')
df <- setDT(as.data.frame(df))
#fill in daily close of call and put
for(i in 1:length(Days)){
  df[,paste0(Days[i],'C'):= w.wsd(CCode,"close",Days[i],Days[i])[[2]][[2]]]
  df[,paste0(Days[i],'P'):= w.wsd(PCode,"close",Days[i],Days[i])[[2]][[2]]]
}
#df odering
df <-df[order(df[,2],decreasing=FALSE),] 
cal_ivx <- data.table(df[,c('CCode','K','PCode')])
cal_ivx$K <- as.numeric(as.character(cal_ivx$K))
#for 50etf, delta_K equals 0.05
cal_ivx[,DeltaK:=0.05]

#load risk-free rate from R, we use 7-day Shibor as the risk free rate
rf <- w.edb('M0017139',StartDate,EndDate,'Fill=Previous')[[2]]
rf <- setDT(rf)

#fill processed inputs: risk free rate,time till expiration,f,k0 and p(k) into data table "cal_ivx"
for(i in 1:length(Days)){
  rfr = rf[[i,2]]/100
  t = round(as.numeric(difftime(CEndDate,Days[i],units = c('days'))),0)/365
  f = MinDiffK(df[[4+(i-1)*2]],df[[5+(i-1)*2]],cal_ivx$K)+cmp(df[[4+(i-1)*2]],df[[5+(i-1)*2]])*exp(-rfr*t)
  k0 = round(K0(f,cal_ivx$K),2)
  cal_ivx[1:4,Days[i]:= c(f,k0,rfr,t)]
  cal_ivx[,paste0(Days[i],'_pk'):= PK(cal_ivx[[2,5+(i-1)*2]],cal_ivx$K,df[[4+(i-1)*2]],df[[5+(i-1)*2]])]
}

#calculate var
ivx <- setDT(as.data.frame(Days))
ivx[,NT:= round(as.numeric(difftime(CEndDate,Days,units = c('days'))),0)]
ivx[,t:= NT/365]
for(i in 1:length(Days)){
  f = cal_ivx[[1,5+(i-1)*2]]
  k0 = cal_ivx[[2,5+(i-1)*2]]
  rfr = cal_ivx[[3,5+(i-1)*2]]
  ts= cal_ivx[[4,5+(i-1)*2]]
  pk = cal_ivx[[6+(i-1)*2]]
  deltak = cal_ivx[[4]]
  ivx[i,var:= 2/ts*sum(deltak/(cal_ivx[[2]])^2*exp(rfr*t)*pk)-1/ts*(f/k0-1)^2]
}

# the same procedure for next month's contracts
CCode1 <- as.character(code$c1)
PCode1 <- as.character(code$p1)
K1 <- as.numeric(code$k1)
df <- cbind(CCode1,K1,PCode1)
colnames(df) <- c('CCode','K','PCode')
df <- setDT(as.data.frame(df))
#fill in daily close of call and put
for(i in 1:length(Days)){
  df[,paste0(Days[i],'C'):= w.wsd(CCode,"close",Days[i],Days[i])[[2]][[2]]]
  df[,paste0(Days[i],'P'):= w.wsd(PCode,"close",Days[i],Days[i])[[2]][[2]]]
}
#df odering
df <-df[order(df[,2],decreasing=FALSE),] 
cal_ivx <- data.table(df[,c('CCode','K','PCode')])
cal_ivx$K <- as.numeric(as.character(cal_ivx$K))
cal_ivx[,DeltaK:=0.05]
#calculating rfr,t,f,k0
for(i in 1:length(Days)){
  rfr = rf[[i,2]]/100
  t = round(as.numeric(difftime(CEndDate1,Days[i],units = c('days'))),0)/365
  f = MinDiffK(df[[4+(i-1)*2]],df[[5+(i-1)*2]],cal_ivx$K)+cmp(df[[4+(i-1)*2]],df[[5+(i-1)*2]])*exp(-rfr*t)
  k0 = round(K0(f,cal_ivx$K),2)
  cal_ivx[1:4,Days[i]:= c(f,k0,rfr,t)]
  cal_ivx[,paste0(Days[i],'_pk'):= PK(cal_ivx[[2,5+(i-1)*2]],cal_ivx$K,df[[4+(i-1)*2]],df[[5+(i-1)*2]])]
}

#calculate var1
ivx[,NT1:= round(as.numeric(difftime(CEndDate1,Days,units = c('days'))),0)]
ivx[,t1:= NT1/365]
for(i in 1:length(Days)){
  f = cal_ivx[[1,5+(i-1)*2]]
  k0 = cal_ivx[[2,5+(i-1)*2]]
  rfr = cal_ivx[[3,5+(i-1)*2]]
  ts= cal_ivx[[4,5+(i-1)*2]]
  pk = cal_ivx[[6+(i-1)*2]]
  deltak = cal_ivx[[4]]
  ivx[i,var1:=2/ts*sum(deltak/(cal_ivx[[2]])^2*exp(rfr*t)*pk)-1/ts*(f/k0-1)^2]
}
#calculate ivx
for(i in 1:length(ivx)){
  ivx[,ivx:=100*sqrt(((NT1-30)/(NT1-NT)*t*var+(30-NT)/(NT1-NT)*t1*var1)*365/30)]
}
