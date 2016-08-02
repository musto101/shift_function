#' Deciles Function.
#' 
#' This function Estimate the deciles for the data in vector x
#' using the Harrell-Davis estimate of the qth quantile.
#' 
#' @param x is a numeric vector containing the data needed to estimate
#' the deciles.
#' @return It returns a vector containing the values at deciles 1 to 9 for
#' the data.
#' @export
#' 
deciles <- function(x){
  
  x <- sort(x)
  n <- length(x)
  vecx <- 1:n
  xq <- vector()
  
  for (i in 1:9) {
    q <- i / 10
    m1 <- (n + 1) * q
    m2 <- (n + 1) * (1 - q)
    wx <- pbeta(vecx / n, m1, m2) - pbeta((vecx - 1) / n, m1, m2)  # W sub i values
    xq[i] <- sum(wx * x)
  }
  
  xq
}