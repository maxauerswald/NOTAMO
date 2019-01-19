estimate_quantile_mix <- function(icdf_list,targetmom,acc=7,startval=NULL,alg="NLOPT_GN_CRS2_LM",maxeval=500){
  #estimates a quantile mixture based on a set of functions to conform to prespecified moments

  #prepare output
  out <- list()
  out$solution <- 0
  out$exp_moms <- matrix(0,nrow=dim(targetmom)[1],ncol=dim(targetmom)[2])
  out$exp_moms[,1] <- targetmom[,1]
  presp_functions <- icdf_list

  #set accuracy vector
  p <- seq(10^(-acc),1-10^(-acc),10^(-acc))

  #append accuracy vector to every function
  for (i in 1:length(icdf_list)){
    icdf_list[[i]]$p <- p
  }

  #handle starting values
  if (is.null(startval)) {
    startval <- rep(1,length(icdf_list)-1)/length(icdf_list)
  } else { #else check if starting values have correct size
    if(length(icdf_list)!=(length(startval)+1)) {
      if (length(icdf_list)==(length(startval))) {
        startval <- startval[1:(length(startval)-1)]/sum(startval)
      } else {
        stop('incorrect number of starting values')
      }
    }
  }

  #choose if root finding algorithm or nonlinear optimization is appropriate
  if (length(icdf_list)==(dim(targetmom)[1]+1)) {

    #define function that gets solved in multiroot
    function2solve0 <- function(x,params){
      functions <- params[[1]] #contains functions and parameters for function
      targetmom <- params[[2]]
      out <- rep(0,dim(targetmom)[1])
      for (i in 1:dim(targetmom)[1]){
        out[i] <- calc_std_mom(estimate_distr(abs(x), icdf_list),targetmom[i,1])-targetmom[i,2]
      }
      return(out)
    }

    params <- list(icdf_list,targetmom)
    solution <- multiroot(function2solve0, startval, parms = params, maxiter = maxeval)
    # guarantee that weights are positive and add up to one
    solution$root <- abs(solution$root)
    scaled_sol <- solution$root/(sum(abs(solution$root))+abs(1-sum(solution$root)))

    for (i in 1:dim(targetmom)[1]) {
      out$exp_moms[i,2] <- calc_std_mom(estimate_distr(scaled_sol, icdf_list), targetmom[i,1])
    }
    out$ssd <- sum((out$exp_moms[,2]-targetmom[,2])^2)
    out$solution <- c(scaled_sol,1-sum(scaled_sol))

    if (out$ssd > 0.1) {
      warning(paste('Sum of squared differences between prespecified and expected targetmom is ',out$ssd,
                    '. Try different functions or starting values.',sep=""))
    }

  } else { #nloptr package handles cases with more functions/distributions than prespecified moments

    #define function that gets minimized in nloptr
    function2solve1 <- function(x){
      # output is sum of squared difference between target and generated moments
      out <- rep(0,dim(targetmom)[1])
      for (i in 1:dim(targetmom)[1]){
        out[i] <- calc_std_mom(estimate_distr(x, icdf_list), targetmom[i,1]) - targetmom[i,2]
      }
      return(sum(out^2))
    }

    # apply nonlinear minimization (to minimize absolute difference)
    solution <- nloptr(startval, function2solve1,opts=list(algorithm=alg,xtol_rel=.0001,maxeval=maxeval),
                       lb=rep(0,length(startval)),ub=rep(1,length(startval)))
    # guarantee that weights add up to one
    scaled_sol <- solution$solution/(sum(solution$solution)+abs(1-sum(solution$solution)))

    for (i in 1:dim(targetmom)[1]) {
      out$exp_moms[i,2] <- calc_std_mom(estimate_distr(scaled_sol, icdf_list), targetmom[i,1])
    }
    out$ssd <- sum((out$exp_moms[,2] - targetmom[,2])^2)
    out$solution <- c(scaled_sol,sum(scaled_sol))

    out$h <- solution

    if (out$ssd > 0.1) {
      warning(paste('Sum of squared differences between prespecified and expected moments is ',
                    out$ssd,'. Try different inverse cdfs or starting values.',sep=""))
    }
  }
  out$presp_moms <- targetmom
  out$presp_funs <- presp_functions
  return(out)
}
