#' Shift Function 
#' 
#' This function computes the confidence intervals for the difference
#' between the deciles of two independent groups. The simultaneous 
#' probability coverage is .95. The Harrell-Davis estimate of
#' the qth quantile is used.
#' 
#' @param x is a numeric vector of data, the deciles of which will be computed.
#' @param y is a numeric vector of data, the deciles of which will be computed.
#' @param nboot is a integer value specifying the number of bootstrap samples
#' to compute.
#' @param plotit is a boolean value indicating whether the shift function is
#' to be plotted.
#' @param plotop is a boolean indicating whether the actual values at the
#' deciles of x are to be plotted. If not, the number of the decile is plotted.
#' @param SEED is a boolean indicating whether a seed is to be used for
#' reproducible results. 
#' @return It rerturns a 9 by 3 matrix containing the lower and upper 
#' confidence intervals for the shift function, and the shift function 
#' estimation itself. If plotit is TRUE it also returns a plot of the 
#' shift function.
#' @import parallel
#' @export
#'
  shifthd <- function(x, y, nboot = 200, plotit = TRUE, plotop = FALSE,
                      SEED = TRUE){
    
      # User defined parameters.
      no_cores <- detectCores() - 1 
      cl <- makeCluster(no_cores)
    
          
      x <- na.omit(x)
      y <- na.omit(y)
          
      if(SEED)set.seed(2)
          
      crit <- 80.1 / (min(length(x), length(y))) ^ 2 + 2.73
      m <- matrix(0, 9, 3)
          
      for (i in 1:9){
        q <- i/10
        print("Working on quantile")
        print(q)
        
        dat <- matrix(parSapply(cl = cl, X = x, FUN = sample_func, nboot = nboot),
                      nrow = nboot)
        bvec <- parApply(cl, dat, 1, hd, q)
        sex <- var(bvec)
        
        dat <- matrix(parSapply(cl = cl, X = y, sample_func, nboot = nboot))
        bvec <- parApply(cl = cl, dat, 1, hd, q = q)
        sey <- var(bvec)
        
        dif <- hd(y, q) - hd(x, q)
        m[i,3] <- dif
        m[i,1] <- dif - crit * sqrt(sex + sey)
        m[i,2] <- dif + crit * sqrt(sex + sey)
          }
      
      dimnames(m) <- list(NULL, c("ci.lower", "ci.upper", "Delta.hat"))
          
      if(plotit){
            
        if(plotop){
          xaxis <- c(1:9) / 10
          xaxis <- c(xaxis, xaxis)
          }
        else{
          xaxis <- c(deciles(x), deciles(x))
          }
            
        par(pch = "+")
        yaxis <- c(m[, 1], m[, 2])
                  
        if(!plotop) plot(xaxis, yaxis, ylab = "delta", xlab = "x (first group)")
        
        if(plotop) plot(xaxis, yaxis, ylab="delta", xlab="Deciles")
        
        par(pch = "*")
        
        if(!plotop) points(deciles(x), m[, 3])
        
        if(plotop) points(c(1:9) / 10, m[, 3])
      }
      stopCluster(cl)
          m
  }