#user defined functions for ivx calculation
#function find K of Min difference call and put price
MinDiffK <- function(c,p,k){
  mindiff = abs(c[1]-p[1])
  for (i in 1:length(c)){
    diff = abs(c[i]-p[i])
    if(diff<=mindiff){
      mindiff = diff
      row0 = i
    }
  }
  return(k[row0])
}

#function return the value of call minus put for minimum diff between call and put
cmp <- function(c,p){
  mindiff = abs(c[1]-p[1])
  for (i in 1:length(c)){
    diff = abs(c[i]-p[i])
    if(diff<=mindiff){
      mindiff = diff
      row0 = i
    }
  }
  return(c[row0]-p[row0])
}
#function K0, first strike below the forward index level,F
K0 <- function(f,k){
  mindiff = f - k[1]
  for(i in 1:length(k)){
    diff = f-k[i]
    if(diff<=mindiff&diff>=0){
      mindiff = diff
      row0 = i
    }
  }
  return(k[row0])
}
#function PK, if ki < k0, then pk equals the put price, else if ki > k0, pk equals the call price
#if pi = p0, pk = ki
PK <- function(k0,k,c,p){
  pk = as.array(NA)
  for(i in 1:length(k)){
    if(k[i]>k0){
      pk[i] = c[i]
    }
    else if (k[i]<k0){
      pk[i] = p[i]
    }
    else{
      pk[i] = k0
    }
  }
  return(pk)
}