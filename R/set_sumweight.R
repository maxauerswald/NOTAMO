set_sumweight <- function(cormat,maxcor=0.9) {
  # sets the weight c in X=sqrt(c)L+sqrt(1-c)E so that X can conform with prespecified correlation matrix cormat
  # maxcor = maximum correlation allowed for L
  out <- list()
  diag(cormat) <- 0
  if (max(abs(cormat))<=(maxcor/2)) {
    out$c <- 0.5
    out$Lmat <- cormat*2
    diag(out$Lmat) <- 1
    return(out)
  } else {
    if ((max(abs(cormat))^2/(maxcor^2))>1) {
      stop(paste0('Correlation matrix cannot be reproduced. Try increasing the maximum allowed correlation.'))
    } else {
      out$c <- max(abs(cormat))/maxcor
      out$Lmat <- cormat/out$c
      diag(out$Lmat) <- 1
      return(out)
    }
  }
}

