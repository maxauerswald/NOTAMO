gen_multivar_NOTAMO <- function(estimate_sum_out,N) {
  nvar <- dim(estimate_sum_out$para$Lcor)[1]

  assign("estimate_sum_out", estimate_sum_out, envir = .GlobalEnv)
  if (estimate_sum_out$para$Lnn) { # non-normal var are correlated, normal var are independent

    invcdflist <- list()
    for (j in 1:nvar) {
      invcdflist[[j]] <- estimate_sum_out[[j]]$presp_funs
    }

    # to do: check_input_cormat

    lat <- genNORTARA(N, estimate_sum_out$para$Lcor, invcdfnames=rep("toNortafun",nvar),
               paramslists=as.list(seq(1,nvar)))
    for (i in 1:nvar) {
      lat[,i] <- lat[,i] * estimate_sum_out[[i]]$varfak
    }
    err <- matrix(rnorm(N*nvar),nrow=N,ncol=nvar)

  } else { # non-normal variable are independent, normal variable are correlated

    lat <- matrix(rnorm(N*nvar),nrow=N,ncol=nvar) %*% chol(estimate_sum_out$para$Lcor)

    #append uniform variable to function list
    p <- runif(N)
    for (j in 1:nvar) {
      for (i in 1:length(estimate_sum_out[[j]]$presp_funs)){
        estimate_sum_out[[j]]$presp_funs[[i]][[length(estimate_sum_out[[j]]$presp_funs[[i]])+1]] <- p
      }
    }

    err <- matrix(0,nrow=N,ncol=nvar)
    for (j in 1:nvar) {
      err[,j] <- estimate_distr(estimate_sum_out[[j]]$solution[1:(length(estimate_sum_out[[j]]$solution)-1)],
                                estimate_sum_out[[j]]$presp_funs) * estimate_sum_out[[j]]$varfak
    }

  }

  out.dat <- sqrt(estimate_sum_out$para$c)*lat+sqrt(1-estimate_sum_out$para$c)*err

  return(out.dat)
}



