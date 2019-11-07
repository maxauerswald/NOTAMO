estimate_sum <- function(icdf_list, targetcor, targetmom, Lnn=T, Lmaxcor=NULL, acc=7, startval=NULL, verbose=T) {
  # Estimates apropriate correlation matrix, weights, and quantile mix from which gen_multivar_NOTAMO
  # can sample

  # Default maximum allowed correlation in cor(L)
  if (is.null(Lmaxcor)) {
    if (Lnn) {
      Lmaxcor <- 0.8
    } else {
      Lmaxcor <- 0.99
    }
  }

  # calc the necessary c to conform with correlation matrix
  sws <- set_sumweight(targetcor, Lmaxcor)

  # calc the necessary kurtosis and skewness of E and L
  adj.targetmom <- list()
  for (iii in 1:dim(targetcor)[1]) {
    adj.targetmom[[iii]] <- calc_sum_mom(targetmom[[iii]], Lnn, sws$c)
    if (verbose) {
      for (jjj in 1:dim(adj.targetmom[[iii]])[1]) {
        print(paste0("The ",adj.targetmom[[iii]][jjj,1] , ". adjusted target moment for variable ",iii, " is ", adj.targetmom[[iii]][jjj,2]))
      }
    }
  }

  # estimate quantile mix for every distribution
  out.distrfun <- list()
  for (iii in 1:dim(targetcor)[1]) {
    if (is.null(startval)) {
      out.distrfun[[iii]] <- estimate_quantile_mix(icdf_list[[iii]], adj.targetmom[[iii]], acc=acc)
    } else {
      out.distrfun[[iii]] <- estimate_quantile_mix(icdf_list[[iii]], adj.targetmom[[iii]], acc=acc, startval=startval[[iii]])
    }

    if (verbose) {
      print(paste0("Estimated quantile mix for variable ",iii))
    }
  }

  # ensure that variance is 1
  for (iii in 1:dim(targetcor)[1]) {
    out.distrfun[[iii]]$varfak <- 1/estimate_sd(out.distrfun[[iii]]$solution,
                                              out.distrfun[[iii]]$presp_funs, acc=acc)
  }

  # carry paramter for gen_multivar function
  out.distrfun$para$Lnn <- Lnn
  out.distrfun$para$c <- sws$c
  out.distrfun$para$Lcor <- sws$Lmat

  return(out.distrfun)
}
