calc_std_mom <- function(data,moment) {
  if (any(moment==c(3,4))) {
    if (moment==3) {
      out <- skewness(data)
    } else {
      out <- kurtosis(data)
    }
  } else { # if target moment is not skewness or kurtosis, compute standardized central moment
    out <- moment(data,order=moment,central=TRUE)/(sd(data)^moment)
  }
  return(out)
}
