estimate_sd <- function(a, functions, acc) {
  # set accuracy vector
  ps <- seq(10^(-acc),1-10^(-acc),10^(-acc))

  # append accuracy vector to every function
  for (i in 1:length(functions)){
    functions[[i]] <- append(functions[[i]],list(p=ps),after=length(functions[[i]])-2)
  }
  return(sd(estimate_distr(a[1:(length(a)-1)],functions)))
}
