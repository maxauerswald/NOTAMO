estimate_sd <- function(a, functions, acc) {
  #set accuracy vector
  p <- seq(10^(-acc),1-10^(-acc),10^(-acc))
  
  #append accuracy vector to every function
  for (i in 1:length(functions)){
    functions[[i]]$p <- p
  }
  
  return(sd(estimate_distr(a[1:(length(a)-1)],functions)))
}