deciles<- function(x){
  #
  #  Estimate the deciles for the data in vector x
  #  using the Harrell-Davis estimate of the qth quantile
  #
  xs<-sort(x)
  n<-length(x)
  vecx<-seq(along=x)
  xq<-0
  for (i in 1:9){
    q<-i/10
    m1<-(n+1)*q
    m2<-(n+1)*(1-q)
    wx<-pbeta(vecx/n,m1,m2)-pbeta((vecx-1)/n,m1,m2)  # W sub i values
    xq[i]<-sum(wx*xs)
  }
  xq
}