calc_sum_mom <- function(targetmom, Lnn, cw) {
  # calculates the necessary standardized central moments of either L or E to conform with prespecified
  # moments of X
  # Lnn=Latent variable is non-normal
  if (any(targetmom[,1]>=7)) {
    stop(paste0('Moments of order 7 and beyond are not yet implemented.'))
  }
  internalmom <- targetmom
  if (!Lnn) {
    cw <- 1-cw
  }
  for (iii in 1:dim(targetmom)[1]) {
    if (targetmom[iii,1]==3) {
      internalmom[iii,2] <- targetmom[iii,2]/sqrt(cw^3)
    }
    if (targetmom[iii,1]==4) {
      internalmom[iii,2] <- (targetmom[iii,2]+3*(cw^2-1))/(cw^2)
    }
    if (targetmom[iii,1]==5) {
      # requires skewness to be specified
      sk.l <- internalmom[internalmom[,1]==3,2]
      sk.x <- targetmom[targetmom[,1]==3,2]
      internalmom[iii,2] <- (targetmom[iii,2]-10*sk.x)/(sqrt(cw^5))+10*sk.l
    }
    if (targetmom[iii,1]==6) {
      # requires skewness and kurtosis to be specified
      sk.l <- internalmom[internalmom[,1]==3,2]
      sk.x <- targetmom[targetmom[,1]==3,2]
      ku.l <- internalmom[internalmom[,1]==4,2]
      ku.x <- targetmom[targetmom[,1]==4,2]
      h <- targetmom[iii,2] - 15*(1-cw^2)^3 - 15*ku.x - 10*sk.x^2 + 30
      internalmom[iii,2] <- h/(cw^3) + 15*ku.l + 10*sk.l^2 - 30
    }

  }
  return(internalmom)

}

