gen_multivar_NOTAMO <- function(estimate_sum_out,N) {
  nvar <- dim(estimate_sum_out$para$Lcor)[1]

  assign("estimate_sum_out", estimate_sum_out, envir = .GlobalEnv)
  if (estimate_sum_out$para$Lnn) { # non-normal var are correlated, normal var are independent

    lat <- genNORTARA(N, estimate_sum_out$para$Lcor, invcdfnames=rep("toNortafun",nvar),
               paramslists=as.list(seq(1,nvar)))
    for (i in 1:nvar) {
      lat[,i] <- lat[,i] * estimate_sum_out[[i]]$varfak
    }
    err <- matrix(rnorm(N*nvar),nrow=N,ncol=nvar)

  } else { # non-normal variable are independent, normal variable are correlated

    lat <- matrix(rnorm(N*nvar),nrow=N,ncol=nvar) %*% chol(estimate_sum_out$para$Lcor)

    err <- matrix(0,nrow=N,ncol=nvar)
    for (j in 1:nvar) {
      err[,j] <- gen_univar_NOTAMO(estimate_sum_out[[j]], N) * (estimate_sum_out[[j]]$varfak)
    }
  }
  out.dat <- sqrt(estimate_sum_out$para$c)*lat+sqrt(1-estimate_sum_out$para$c)*err

  return(out.dat)
}
