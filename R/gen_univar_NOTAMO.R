gen_univar_NOTAMO <- function(quantile_mix_out,N) {
  # takes the output of quantile_mix to generate a sample of size N, according to the prespecified functions and
  # moments
  ps <- runif(N)

  #append uniform variable to function list
  for (i in 1:length(quantile_mix_out$presp_funs)){
    quantile_mix_out$presp_funs[[i]] <- append(quantile_mix_out$presp_funs[[i]],list(p=ps),after=length(quantile_mix_out$presp_funs[[i]])-2)
  }

  return(estimate_distr(quantile_mix_out$solution[1:(length(quantile_mix_out$solution)-1)],
                        quantile_mix_out$presp_funs))
}
