#' Sample Function
#' 
#' This function allows for sampling with replacement a vector of data.
#' 
#' @param x is a numeric vector indicating the data to be sampled.
#' @param nboot is the number of bootstrap samples to take. 
#' @return It returns an n by n matrix containing the bootstrapped samples.
#' @export
#' 
sample_func <- function(x, nboot = 200){
  sample(x, size=length(x) * nboot,
         replace = TRUE)
}
