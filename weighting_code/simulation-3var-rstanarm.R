library(rstanarm)
library(survey)
library(dplyr)
library(foreign)

if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("stan-dev/rstanarm", args = "--preclean", build_vignettes = FALSE, ref = 'structured_prior_merge')
set.seed(20150213)

source('cell_weights.R')

#' @param object rstanarm fit
#' @param agg_pop poststrat frame
sum_svey_model <- function(object, agg_pop) {
  model_data <- object$data
  cell_table <- model_data[,c('N','n')]
  ret_list <- list(mu_cell = rstanarm::posterior_linpred(object, newdata = model_data),
                   mu_cell_pred = rstanarm::posterior_linpred(object, newdata = agg_pop),
                   w_new = model_based_cell_weights(object, cell_table))
  colnames(ret_list$mu_cell_pred) <- agg_pop$cell_id
  colnames(ret_list$mu_cell) <- model_data$cell_id
  ret_list$theta_sample <- ret_list$mu_cell %*% (cell_table$N / sum(cell_table$N))
  ret_list$theta_pred <- ret_list$mu_cell_pred %*% (agg_pop$N / sum(agg_pop$N))
  ret_list$mean_w_new <- data.frame(w_unit = colMeans(ret_list$w_new), 
                                    cell_id = model_data$cell_id)
  return(ret_list)
}

sum_weights <- function(weight_df, idx, comp_stat) {
  sub_weight_df <- weight_df %>% filter(cell_id %in% idx)
  Y_sub <- sub_weight_df$Y
  Y_w_sub <- sub_weight_df$Y_w
  w_sub <- sub_weight_df$w
  est_wt <- sum(Y_w_sub)/sum(w_sub)
  bias <- est_wt - comp_stat
  sd_wt <- sqrt(sum(w_sub^2 * var(Y_sub)))/sum(w_sub)
  cr_wt <- as.numeric(est_wt - 1.96 * sd_wt <= comp_stat & comp_stat <= est_wt +
                                        1.96 * sd_wt)
  return(list(bias = bias, sd_wt = sd_wt, cr_wt = cr_wt))
}

acs_pop <- read.dta("data/acs_nyc_2011_wpov1.dta", convert.factors = FALSE)
  
acs_ad <- acs_pop %>% filter(age >= 18) %>%
  mutate(
    age_gp = cut(x = age,
                  breaks = c(0,34,44,54,64,Inf),
                  right = TRUE),
    age_dc = as.integer(age_gp),
    race_dc = racex,
    opmres_gp = cut(x = poverty,
                    breaks = c(0,50,100,200,300,Inf),
                    right = TRUE),
    opmres_x = as.integer(opmres_gp),
    eldx_ca = if_else(eldx > 1, 3, eldx + 1),
    childx_ca = if_else(childx > 2, 4, childx + 1),
    personx_ca = if_else(personx > 4,4,personx)
  ) %>% select(-age_gp, -opmres_gp)

q <- 3  # #weighting variables

J_age <- length(unique(acs_ad$age_dc))
J_eth <- length(unique(acs_ad$race_dc))
J_edu <- length(unique(acs_ad$educat))
J_true <- J_age * J_eth * J_edu
N <- dim(acs_ad)[1]

acs_ad$age_dc <- as.factor(acs_ad$age_dc)
acs_ad$race_dc <- as.factor(acs_ad$race_dc)
acs_ad$educat <- as.factor(acs_ad$educat)

options("contrasts")
age <- model.matrix(~age_dc, acs_ad)
race <- model.matrix(~race_dc, acs_ad)
edu <- model.matrix(~educat, acs_ad)

age_race <- model.matrix(~age_dc:race_dc, acs_ad)[, -1]
age_edu <- model.matrix(~age_dc:educat, acs_ad)[, -1]
race_edu <- model.matrix(~race_dc:educat, acs_ad)[, -1]

age_race_edu <- model.matrix(~age_dc:race_dc:educat, acs_ad)[, -1]

### simulate Y case 1)
betaY_age <- matrix(seq(0.5, 4, length = dim(age)[2]), dim(age)[2], 1)
betaY_race <- matrix(seq(-2, 2, length = dim(race)[2]), dim(race)[2], 1)
betaY_edu <- matrix(seq(3, 0, length = dim(edu)[2]), dim(edu)[2], 1)

betaY_age_race <- matrix(sample(1:4, dim(age_race)[2], replace = T), dim(age_race)[2], 1)
betaY_age_edu <- matrix(sample(-2:2, dim(age_edu)[2], replace = T), dim(age_edu)[2], 1)
betaY_race_edu <- matrix(sample(-3:1, dim(race_edu)[2], replace = T), dim(race_edu)[2], 1)
betaY_age_race_edu <- matrix(sample(seq(-1, 1, by = 0.5), dim(age_race_edu)[2], replace = T), dim(age_race_edu)[2], 
                             1)

muY <- age %*% betaY_age + race %*% betaY_race + edu %*% betaY_edu + age_race %*% betaY_age_race + age_edu %*% 
    betaY_age_edu + race_edu %*% betaY_race_edu + age_race_edu %*% betaY_age_race_edu

Y <- rnorm(N) + muY

betaI_age <- matrix(seq(-2, 0, length = dim(age)[2]), dim(age)[2], 1)
betaI_race <- matrix(seq(-1, 1, length = dim(race)[2]), dim(race)[2], 1)
betaI_edu <- matrix(seq(0, 3, length = dim(edu)[2]), dim(edu)[2], 1)

betaI_age_race <- matrix(0, dim(age_race)[2], 1)
betaI_age_edu <- matrix(0, dim(age_edu)[2], 1)
betaI_race_edu <- matrix(0, dim(race_edu)[2], 1)
betaI_age_race_edu <- matrix(0, dim(age_race_edu)[2], 1)

sel_prob <- 1/(1 + exp(-(-2 + age %*% betaI_age + race %*% betaI_race + edu %*% betaI_edu + age_race %*% betaI_age_race + 
    age_edu %*% betaI_age_edu + race_edu %*% betaI_race_edu + age_race_edu %*% betaI_age_race_edu)))

pop_data <- data.frame(age = acs_ad$age_dc, eth = acs_ad$race_dc, edu = acs_ad$educat, Y = Y) %>%
  mutate(pop_cell_id = paste0(age, eth, edu))

pop_cell_id <- pop_data$pop_cell_id
cell_str <- expand.grid(age = 1:J_age, eth = 1:J_eth, edu = 1:J_edu) 
pstrat_cell_ids <- cell_str %>% mutate(pop_cell_id = paste0(age, eth, edu)) %>% .$pop_cell_id
J_true <- J_age * J_eth * J_edu

# Aggregates population data by poststratification cell
agg_pop <- pop_data %>% group_by(age, eth, edu) %>%
  summarise(N = n(),
            Y = mean(Y),
            cell_id = first(pop_cell_id)) %>%
  ungroup() %>% mutate(
    age = as.factor(age),
    eth = as.factor(eth),
    edu = as.factor(edu)
    )

R <- 3
n_r <- rep(0, R)
bias_mu_pred <- rep(0, R)
sd_mu_pred <- rep(0, R)
cr_mu_pred <- rep(0, R)
bias_mu_sample <- rep(0, R)
sd_mu_sample <- rep(0, R)
cr_mu_sample <- rep(0, R)
bias_mu_popcell <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
sd_mu_popcell <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
cr_mu_popcell <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
bias_mu_cell <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
sd_mu_cell <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
cr_mu_cell <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))

bias_mu_pred_iid <- rep(0, R)
sd_mu_pred_iid <- rep(0, R)
cr_mu_pred_iid <- rep(0, R)
bias_mu_sample_iid <- rep(0, R)
sd_mu_sample_iid <- rep(0, R)
cr_mu_sample_iid <- rep(0, R)
bias_mu_popcell_iid <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
sd_mu_popcell_iid <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
cr_mu_popcell_iid <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
bias_mu_cell_iid <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
sd_mu_cell_iid <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))
cr_mu_cell_iid <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))

bias_mu_st <- rep(0, R)
sd_mu_st <- rep(0, R)
cr_mu_st <- rep(0, R)
bias_mu_id <- rep(0, R)
sd_mu_id <- rep(0, R)
cr_mu_id <- rep(0, R)
bias_mu_ps <- rep(0, R)
sd_mu_ps <- rep(0, R)
cr_mu_ps <- rep(0, R)
bias_mu_ips <- rep(0, R)
sd_mu_ips <- rep(0, R)
cr_mu_ips <- rep(0, R)
bias_mu_rake <- rep(0, R)
sd_mu_rake <- rep(0, R)
cr_mu_rake <- rep(0, R)

wght_sd_rt <- matrix(0, R, 2)
wght_sd_rt_ps <- matrix(0, R, 2)
wght_sd_rt_ips <- matrix(0, R, 2)
wght_sd_rt_rake <- matrix(0, R, 2)
wght_sd_rt_iid <- matrix(0, R, 2)

l_v <- c(0, J_age, J_eth, J_edu)

bias_sub_st <- matrix(0, R, sum(l_v))
sd_sub_st <- matrix(0, R, sum(l_v))
cr_sub_st <- matrix(0, R, sum(l_v))
bias_sub_st_wt <- matrix(0, R, sum(l_v))
sd_sub_st_wt <- matrix(0, R, sum(l_v))
cr_sub_st_wt <- matrix(0, R, sum(l_v))
bias_sub_iid <- matrix(0, R, sum(l_v))
sd_sub_iid <- matrix(0, R, sum(l_v))
cr_sub_iid <- matrix(0, R, sum(l_v))
bias_sub_iid_wt <- matrix(0, R, sum(l_v))
sd_sub_iid_wt <- matrix(0, R, sum(l_v))
cr_sub_iid_wt <- matrix(0, R, sum(l_v))
bias_sub_ps_wt <- matrix(0, R, sum(l_v))
sd_sub_ps_wt <- matrix(0, R, sum(l_v))
cr_sub_ps_wt <- matrix(0, R, sum(l_v))
bias_sub_ips_wt <- matrix(0, R, sum(l_v))
sd_sub_ips_wt <- matrix(0, R, sum(l_v))
cr_sub_ips_wt <- matrix(0, R, sum(l_v))
bias_sub_rake_wt <- matrix(0, R, sum(l_v))
sd_sub_rake_wt <- matrix(0, R, sum(l_v))
cr_sub_rake_wt <- matrix(0, R, sum(l_v))

bias_sub_st_int <- rep(0, R)
sd_sub_st_int <- rep(0, R)
cr_sub_st_int <- rep(0, R)
bias_sub_st_wt_int <- rep(0, R)
sd_sub_st_wt_int <- rep(0, R)
cr_sub_st_wt_int <- rep(0, R)
bias_sub_iid_int <- rep(0, R)
sd_sub_iid_int <- rep(0, R)
cr_sub_iid_int <- rep(0, R)
bias_sub_iid_wt_int <- rep(0, R)
sd_sub_iid_wt_int <- rep(0, R)
cr_sub_iid_wt_int <- rep(0, R)
bias_sub_ps_wt_int <- rep(0, R)
sd_sub_ps_wt_int <- rep(0, R)
cr_sub_ps_wt_int <- rep(0, R)
bias_sub_ips_wt_int <- rep(0, R)
sd_sub_ips_wt_int <- rep(0, R)
cr_sub_ips_wt_int <- rep(0, R)
bias_sub_rake_wt_int <- rep(0, R)
sd_sub_rake_wt_int <- rep(0, R)
cr_sub_rake_wt_int <- rep(0, R)

ff <- as.formula(Y ~ 1 + (1 | age) + (1 | eth) + (1 | edu) + (1 | age:eth) + (1 | eth:edu) + (1 | age:edu) + (1 | age:edu:eth))

for (r in 1:R) {
  set.seed(20150213 + r)
  
  ### sampling
  I <- (runif(N) <= sel_prob)
  
  dat <- data.frame(Y = Y[I], age = acs_ad$age_dc[I], eth = acs_ad$race_dc[I], edu = acs_ad$educat[I], cell_id = pop_cell_id[I])
  dat %>% group_by(age, eth, edu) %>% summarise(sd_cell = sd(Y), Y = mean(Y), n = n(), cell_id = first(cell_id),
                                                sd_cell = if_else(is.na(sd_cell),0,sd_cell)) -> dat_rstanarm
  dat_rstanarm %>% ungroup() %>% 
    mutate(
      age = as.factor(age),
      edu = as.factor(edu),
      eth = as.factor(eth)
    ) %>% 
    left_join(
      agg_pop[,c('cell_id','N')], 
    by = 'cell_id'
  ) -> dat_rstanarm
  #-----------computation--------------#
  ### cell id
  n <- dim(dat)[1]  #sample size
  cell_id <- dat_rstanarm$cell_id
  
  ###-----------------STAN with structural prior--------------------------###
  
  S_arm <- rstanarm::stan_glmer(formula = ff, data = dat_rstanarm, iter = 500, chains = 4, cores = 4, 
                            prior_covariance = rstanarm::mrp_structured(cell_size = dat_rstanarm$n, cell_sd = dat_rstanarm$sd_cell), 
                            seed = 123, prior_aux = cauchy(0,5), prior_intercept = normal(0, 100, autoscale = FALSE))
  output <- sum_svey_model(S_arm, agg_pop)
  dat %>%
    left_join(
      output$mean_w_new, by = 'cell_id'
  ) %>% mutate(
    w_unit = w_unit / mean(w_unit),
    Y_w = w_unit * Y
  ) %>% select(cell_id, w_unit, Y_w, Y) -> st_out
  output$mu_w <- mean(st_out$Y_w)
  output$w_unit <- st_out$w_unit
  
  # prediction also using empty cells
  bias_mu_pred[r] <- mean(output$theta_pred) - mean(Y)
  sd_mu_pred[r] <- sd(output$theta_pred)
  if (quantile(output$theta_pred, 0.025) <= mean(Y) & mean(Y) <= quantile(output$theta_pred, 0.975)) {
      cr_mu_pred[r] <- 1
  }
  # prediction using only nonempty cells
  bias_mu_sample[r] <- mean(output$theta_sample) - mean(Y)
  sd_mu_sample[r] <- sd(output$theta_sample)
  if (quantile(output$theta_sample, 0.025) <= mean(Y) & mean(Y) <= quantile(output$theta_sample, 0.975)) {
      cr_mu_sample[r] <- 1
  }
  
  ### population cell mean
  bias_mu_popcell[r, colnames(output$mu_cell_pred)] <- colMeans(output$mu_cell_pred) - agg_pop$Y
  sd_mu_popcell[r, colnames(output$mu_cell_pred)] <- apply(output$mu_cell_pred, 2, sd)
  cr_mu_popcell[r, colnames(output$mu_cell_pred)] <- as.numeric(apply(output$mu_cell_pred, 2, quantile, 0.025) <= agg_pop$Y & agg_pop$Y <= 
      apply(output$mu_cell_pred, 2, quantile, 0.975))
  
  ### sampled cell mean
  bias_mu_cell[r, colnames(output$mu_cell)] <- colMeans(output$mu_cell) - dat_rstanarm$Y
  sd_mu_cell[r, colnames(output$mu_cell)] <- apply(output$mu_cell, 2, sd)
  cr_mu_cell[r, colnames(output$mu_cell)] <- as.numeric(apply(output$mu_cell, 2, quantile, 0.025) <= dat_rstanarm$Y & dat_rstanarm$Y <= 
      apply(output$mu_cell, 2, quantile, 0.975))
  
  ### weights
  wght_sd_rt[r, ] <- c(sd(output$w_unit), max(output$w_unit)/min(output$w_unit))
  
  w_sum <- sum_weights(weight_df = st_out, idx = st_out$cell_id, comp_stat = mean(Y))
  bias_mu_st[r] <- w_sum$bias
  sd_mu_st[r] <- w_sum$sd_wt
  cr_mu_st[r] <- w_sum$cr_wt
  
  ###-----------------STAN with independent prior--------------------------###
  
  
  S1 <- rstanarm::stan_glmer(formula = ff, data = dat_rstanarm, iter = 500, chains = 4, cores = 4, seed = 123,
                             prior_covariance = rstanarm::mrp_structured(indep = TRUE, cell_size = dat_rstanarm$n, 
                                                                         cell_sd = dat_rstanarm$sd_cell), 
                             prior_aux = cauchy(0,5))
  output_iid <- sum_svey_model(object = S1, agg_pop = agg_pop)
  dat %>%
    left_join(
      output_iid$mean_w_new, by = 'cell_id'
  ) %>% mutate(
    w_unit = w_unit / mean(w_unit),
    Y_w = w_unit * Y
  ) %>% select(cell_id, w_unit, Y_w, Y) -> st_iid
  output_iid$mu_w <- mean(st_iid$Y_w)
  output_iid$w_unit <- st_iid$w_unit
 
  ###-------------------------------------###
  
  # prediction also using zero cells
  bias_mu_pred_iid[r] <- mean(output_iid$theta_pred) - mean(Y)
  sd_mu_pred_iid[r] <- sd(output_iid$theta_pred)
  if (quantile(output_iid$theta_pred, 0.025) <= mean(Y) & mean(Y) <= quantile(output_iid$theta_pred, 0.975)) {
    cr_mu_pred_iid[r] <- 1
  }
  # prediction using only nonzero cells
  bias_mu_sample_iid[r] <- mean(output_iid$theta_sample) - mean(Y)
  sd_mu_sample_iid[r] <- sd(output_iid$theta_sample)
  if (quantile(output_iid$theta_sample, 0.025) <= mean(Y) & mean(Y) <= quantile(output_iid$theta_sample, 0.975)) {
    cr_mu_sample_iid[r] <- 1
  }
  ### population cell mean
  bias_mu_popcell_iid[r, colnames(output_iid$mu_cell_pred)] <- colMeans(output_iid$mu_cell_pred) - agg_pop$Y
  sd_mu_popcell_iid[r, colnames(output_iid$mu_cell_pred)] <- apply(output_iid$mu_cell_pred, 2, sd)
  cr_mu_popcell_iid[r, colnames(output_iid$mu_cell_pred)] <- as.numeric(apply(output_iid$mu_cell_pred, 2, quantile, 0.025) <= agg_pop$Y & 
                                         agg_pop$Y <= apply(output_iid$mu_cell_pred, 2, quantile, 0.975))
  
  ### sampled cell mean
  bias_mu_cell_iid[r, colnames(output_iid$mu_cell)] <- colMeans(output_iid$mu_cell) - dat_rstanarm$Y
  sd_mu_cell_iid[r, colnames(output_iid$mu_cell)] <- apply(output_iid$mu_cell, 2, sd)
  cr_mu_cell_iid[r, colnames(output_iid$mu_cell)] <- as.numeric(apply(output_iid$mu_cell, 2, quantile, 0.025) <= dat_rstanarm$Y & dat_rstanarm$Y <= 
                                           apply(output_iid$mu_cell, 2, quantile, 0.975))
  
  ### weights
  wght_sd_rt_iid[r, ] <- c(sd(output_iid$w_unit), max(output_iid$w_unit)/min(output_iid$w_unit))
  
  w_sum <- sum_weights(weight_df = st_iid, idx = st_iid$cell_id, comp_stat = mean(Y))
  bias_mu_id[r] <- w_sum$bias
  sd_mu_id[r] <- w_sum$sd_wt
  cr_mu_id[r] <- w_sum$cr_wt
  
  ###------PS weighting------###
  w_ps_df <- dat[,c('cell_id','Y')] %>%
    left_join(agg_pop[,c('cell_id','N')], by = 'cell_id') %>%
    left_join(dat_rstanarm[,c('cell_id','n')], by = 'cell_id') %>%
    mutate(w_ps = N / n,
           w_ps = w_ps / mean(w_ps),
           Y_w = w_ps * Y)
  w_ps <- w_ps_df %>% .$w_ps
  wght_sd_rt_ps[r, ] <- c(sd(w_ps), max(w_ps)/min(w_ps))

  w_sum <- sum_weights(weight_df = w_ps_df, idx = w_ps_df$cell_id, comp_stat = mean(Y))
  bias_mu_ps[r] <- w_sum$bias
  sd_mu_ps[r] <- w_sum$sd_wt
  cr_mu_ps[r] <- w_sum$cr_wt
  
  ###------inverse-prob weighted estimator------###
  w_ips <- 1/sel_prob[I]/mean(1/sel_prob[I])
  w_ips_df <- data.frame(w_ips = w_ips, 
                         Y = dat$Y,
                         cell_id = dat$cell_id) %>%
    mutate(
      Y_w = w_ips * Y
    )

  wght_sd_rt_ips[r, ] <- c(sd(w_ips), max(w_ips)/min(w_ips))

  w_sum <- sum_weights(weight_df = w_ips_df, idx = w_ips_df$cell_id, comp_stat = mean(Y))
  bias_mu_ips[r] <- w_sum$bias
  sd_mu_ips[r] <- w_sum$sd_wt
  cr_mu_ips[r] <- w_sum$cr_wt
  
  ###------raking estimator------###
  dat.design <- svydesign(id = ~1, data = dat)

  pop.age <- data.frame(1:J_age, Freq = as.numeric(table(acs_ad$age_dc)))
  names(pop.age) <- c("age", "Freq")
  pop.eth <- data.frame(1:J_eth, Freq = as.numeric(table(acs_ad$race_dc)))
  names(pop.eth) <- c("eth", "Freq")
  pop.edu <- data.frame(1:J_edu, Freq = as.numeric(table(acs_ad$educat)))
  names(pop.edu) <- c("edu", "Freq")
  dat_rake <- rake(dat.design, list(~age, ~eth, ~edu), list(pop.age, pop.eth, pop.edu))

  w_rake <- weights(dat_rake)/mean(weights(dat_rake))
  w_rake_df <- data.frame(w_rake = w_rake,
                          Y = dat$Y,
                          cell_id = dat$cell_id) %>%
    mutate(
      Y_w = w_rake * Y
    )

  wght_sd_rt_rake[r, ] <- c(sd(w_rake), max(w_rake)/min(w_rake))

  w_sum <- sum_weights(weight_df = w_rake_df, idx = w_rake_df$cell_id, comp_stat = mean(Y))
  bias_mu_rake[r] <- w_sum$bias
  sd_mu_rake[r] <- w_sum$sd_wt
  cr_mu_rake[r] <- w_sum$cr_wt

  ### sub domain
  cell_str <- agg_pop[,c('age','eth','edu')]
  
  for (v in 1:q) {
    for (l in 1:l_v[v + 1]) {
      sub_pop_data <- agg_pop[which(cell_str[,v] == l),c('cell_id','Y','N')] 
      sub_cell_idx <- sub_pop_data$cell_id
      mar_true <- sum(sub_pop_data$Y * sub_pop_data$N/sum(sub_pop_data$N))
      # model prediction
      st_est_sm <- output$mu_cell_pred[,sub_cell_idx] %*% (sub_pop_data$N / sum(sub_pop_data$N))
      bias_sub_st[r, l + sum(l_v[1:v])] <- mean(st_est_sm) - mar_true
      sd_sub_st[r, l + sum(l_v[1:v])] <- sd(st_est_sm)
      if (quantile(st_est_sm, 0.025) <= mar_true & mar_true <= quantile(st_est_sm, 0.975)) {
        cr_sub_st[r, l + sum(l_v[1:v])] <- 1
      }

      # independent prior
      iid_est_sm <- output_iid$mu_cell_pred[,sub_cell_idx] %*%
                                          (sub_pop_data$N / sum(sub_pop_data$N))
      bias_sub_iid[r, l + sum(l_v[1:v])] <- mean(iid_est_sm) - mar_true
      sd_sub_iid[r, l + sum(l_v[1:v])] <- sd(iid_est_sm)
      if (quantile(iid_est_sm, 0.025) <= mar_true & mar_true <= quantile(iid_est_sm, 0.975)) {
        cr_sub_iid[r, l + sum(l_v[1:v])] <- 1
      }
      w_sum <- sum_weights(weight_df = st_out, idx = sub_cell_idx, comp_stat = mar_true)
      # model-based weights under st prior
      bias_sub_st_wt[r, l + sum(l_v[1:v])] <- w_sum$bias
      sd_sub_st_wt[r, l + sum(l_v[1:v])] <- w_sum$sd_wt
      cr_sub_st_wt[r, l + sum(l_v[1:v])] <- w_sum$cr_wt

      # model-based weights under independent prior
      w_sum <- sum_weights(weight_df = st_iid, idx = sub_cell_idx, comp_stat = mar_true)
      bias_sub_iid_wt[r, l + sum(l_v[1:v])] <- w_sum$bias
      sd_sub_iid_wt[r, l + sum(l_v[1:v])] <- w_sum$sd_wt
      cr_sub_iid_wt[r, l + sum(l_v[1:v])] <- w_sum$cr_wt

      
      w_sum <- sum_weights(weight_df = w_ps_df, idx = sub_cell_idx, comp_stat = mar_true)
      # ps weights
      bias_sub_ps_wt[r, l + sum(l_v[1:v])] <- w_sum$bias
      sd_sub_ps_wt[r, l + sum(l_v[1:v])] <- w_sum$sd_wt
      cr_sub_ps_wt[r, l + sum(l_v[1:v])] <- w_sum$cr_wt
      
      w_sum <- sum_weights(weight_df = w_ips_df, idx = sub_cell_idx, comp_stat = mar_true)
      # ips weights
      bias_sub_ips_wt[r, l + sum(l_v[1:v])] <- w_sum$bias
      sd_sub_ips_wt[r, l + sum(l_v[1:v])] <- w_sum$sd_wt
      cr_sub_ips_wt[r, l + sum(l_v[1:v])] <- w_sum$cr_wt

      w_sum <- sum_weights(weight_df = w_rake_df, idx = sub_cell_idx, comp_stat = mar_true)
      # rake weights
      bias_sub_rake_wt[r, l + sum(l_v[1:v])] <- w_sum$bias
      sd_sub_rake_wt[r, l + sum(l_v[1:v])] <- w_sum$sd_wt
      cr_sub_rake_wt[r, l + sum(l_v[1:v])] <- w_sum$cr_wt

    }
  }
  # interaction
  sub_pop_data <- agg_pop[which(cell_str[, 1] == 1 & cell_str[, 2] != 1),c('cell_id','Y','N')] 
  sub_cell_idx <- sub_pop_data$cell_id
  mar_true <- sum(sub_pop_data$Y * sub_pop_data$N)/sum(sub_pop_data$N)

  # model prediction
  st_est_sm <- output$mu_cell_pred[,sub_cell_idx] %*% (sub_pop_data$N / sum(sub_pop_data$N))
  bias_sub_st_int[r] <- mean(st_est_sm) - mar_true
  sd_sub_st_int[r] <- sd(st_est_sm)
  if (quantile(st_est_sm, 0.025) <= mar_true & mar_true <= quantile(st_est_sm, 0.975)) {
    cr_sub_st_int[r] <- 1
  }

  # independent prior
  iid_est_sm <- output_iid$mu_cell_pred[,sub_cell_idx] %*% 
                (sub_pop_data$N / sum(sub_pop_data$N))
  bias_sub_iid_int[r] <- mean(iid_est_sm) - mar_true
  sd_sub_iid_int[r] <- sd(iid_est_sm)
  if (quantile(iid_est_sm, 0.025) <= mar_true & mar_true <= quantile(iid_est_sm, 0.975)) {
    cr_sub_iid_int[r] <- 1
  }

  # model-based weights under st prior
  w_sum <- sum_weights(weight_df = st_out, idx = sub_cell_idx, comp_stat = mar_true)
  bias_sub_st_wt_int[r] <- w_sum$bias
  sd_sub_st_wt_int[r] <- w_sum$sd_wt
  cr_sub_st_wt_int[r] <- w_sum$cr_wt

  # model-based weights under independent prior
  w_sum <- sum_weights(weight_df = st_iid, idx = sub_cell_idx, comp_stat = mar_true)
  bias_sub_iid_wt_int[r] <- w_sum$bias
  sd_sub_iid_wt_int[r] <- w_sum$sd_wt
  cr_sub_iid_wt_int[r] <- w_sum$cr_wt

  # ps weights
  w_sum <- sum_weights(weight_df = w_ps_df, idx = sub_cell_idx, comp_stat = mar_true)
  bias_sub_ps_wt_int[r] <- w_sum$bias
  sd_sub_ps_wt_int[r] <- w_sum$sd_wt
  cr_sub_ps_wt_int[r] <- w_sum$cr_wt
  
  # ips weights
  w_sum <- sum_weights(weight_df = w_ips_df, idx = sub_cell_idx, comp_stat = mar_true)
  bias_sub_ips_wt_int[r] <- w_sum$bias
  sd_sub_ips_wt_int[r] <- w_sum$sd_wt
  cr_sub_ips_wt_int[r] <- w_sum$cr_wt

  # rake weights
  w_sum <- sum_weights(weight_df = w_rake_df, idx = sub_cell_idx, comp_stat = mar_true)
  bias_sub_rake_wt_int[r] <- w_sum$bias
  sd_sub_rake_wt_int[r] <- w_sum$sd_wt
  cr_sub_rake_wt_int[r] <- w_sum$cr_wt
     
}  #end of repeated sampling
