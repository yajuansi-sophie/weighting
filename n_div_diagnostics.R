# stan_fit: stanfit object returned by rstan::stan
# returns: sum of n_divergent draws
get_n_div <- function(stan_fit){
  fitted_samp_params <- get_sampler_params(stan_fit)
  return(sum(mapply(function(chain_i,args_i) sum(chain_i[(args_i[5]$warmup+1):args_i[2]$iter,'n_divergent__']),fitted_samp_params,stan_fit@stan_args)))
}
  
# stan_fit: stanfit object returned by rstan::stan
# pars_vec: character vector of parameters of interest
# e.g. c('sigma_y','sigma_gamma','lp__')
# returns: pairs plot stratified by n_divergent draws
pairs_plot_n_div <- function(stan_fit,pars_vec){
  print(pairs(stan_fit,pars=pars_vec,condition='n_d'))
}