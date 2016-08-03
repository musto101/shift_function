#' hd Function
#' 
#' This function computes the Harrell-Davis estimate of the qth quantile.
#' 
#' @param x is a vector containing the data to be used in the computation.
#' @param q is an float indicating the quantile to be computed.
#' the default argument is .5.
#' @return It returns a single numeric value indicating the value in the data
#' at the quantile specified.
#' @export
#' 
hd <- function(x, q = .5){
  
          n <- length(x)
          m1 <- (n + 1) * q 
          m2 <- (n + 1) * (1 - q)
          vec <- 1:n
          w <- pbeta(vec / n, m1, m2) - pbeta((vec - 1) / n, m1, m2)  # W sub i values
          sum(w*sort(x))
          
}