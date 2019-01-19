estimate_distr <- function(a, functions){
  # Calculates the resulting distribution given a set of functions and weights.
  # a = weights
  # functions = list of functions
  # length(a) = length(functions-1); final fn  weighted by complentary a's
  if(length(a) != (length(functions)-1)) stop('arg mismatch')

  out <- 0
  # build first part of sum: (abs(a1)*f1(p) + abs(a2)*f2(p) + abs(a3)*f3(p)
  for(i in 1 : length(a)){
    # current function
    cf <- functions[[i]][[1]]
    # get args to current function
    args <- list()
    for(j in 2 : length(functions[[i]])){
      args[[j-1]] <- functions[[i]][[j]]
      names(args)[j-1] <- names(functions[[i]])[j]
    }
    # call current function with correct args
    out <- out + abs(a[[i]])*do.call(cf, args)
  }
  # build first and second part of sum with final fn: abs(1-abs(a1)-abs(a2)-abs(a3))*f4(p))
  w <- 1
  for(i in 1 : length(a)){
    w <- w - abs(a[[i]])
  }
  ff <- functions[[length(functions)]][[1]]
  # get args to current function
  args <- list()
  for(j in 2 : length(functions[[length(functions)]])){
    args[[j-1]] <- functions[[length(functions)]][[j]]
    names(args)[j-1] <- names(functions[[length(functions)]])[j]
  }
  out <- out + abs(w)*do.call(ff, args)
  return(out)
}
