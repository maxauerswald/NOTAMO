toNortafun <- function(x,j) {
  out <- 0
  for (i in 1:length(estimate_sum_out[[j]]$solution)) {
    args <- list(p=x)
    if (length(estimate_sum_out[[j]]$presp_funs[[i]])>1) {
      endofpar <- length(estimate_sum_out[[j]]$presp_funs[[i]])
      args <- append(args,estimate_sum_out[[j]]$presp_funs[[i]][[2:endofpar]])
      names(args[2:endofpar]) <- names(estimate_sum_out[[j]]$presp_funs[[i]][[2:endofpar]])
    }
    out <- out + estimate_sum_out[[j]]$solution[i] *
      do.call(estimate_sum_out[[j]]$presp_funs[[i]][[1]],args=args)
  }
  return(out)
}
