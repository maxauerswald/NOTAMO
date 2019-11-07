toNortafun <- function(x,j) {
  # function that makes NOTAMO output suitable for NORTARA package
  out <- 0
  for (i in 1:length(estimate_sum_out[[j]]$solution)) {
    args <- list(p=x)
    if (length(estimate_sum_out[[j]]$presp_funs[[i]])>3) {
      endofpar <- length(estimate_sum_out[[j]]$presp_funs[[i]])-2
      args <- append(args,estimate_sum_out[[j]]$presp_funs[[i]][2:endofpar])
      names(args[2:endofpar]) <- names(estimate_sum_out[[j]]$presp_funs[[i]][2:endofpar])
    }
    out <- out + sqrt(estimate_sum_out[[j]]$solution[i]) *
      (do.call(estimate_sum_out[[j]]$presp_funs[[i]][[1]],args=args)-estimate_sum_out[[j]]$presp_funs[[i]]$notamo.mean)/estimate_sum_out[[j]]$presp_funs[[i]]$notamo.sd
  }
  return(out)
}
