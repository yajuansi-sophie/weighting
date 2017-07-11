library(rstanarm)
library(survey)
library(dplyr)
library(foreign)

set.seed(20150213)

source('weighting_code/cell_weights.R')

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
  mutate(pop_cell_id = (as.integer(edu) - 1) * J_eth * J_age + (as.integer(eth) - 1) * J_age + as.integer(age))

pop_cell_id <- pop_data$pop_cell_id
cell_str <- expand.grid(1:J_age, 1:J_eth, 1:J_edu)
J_true <- J_age * J_eth * J_edu
N_cell_true <- as.numeric(table(pop_cell_id))

# Aggregates population data by poststratification cell
agg_pop <- pop_data %>% group_by(age, eth, edu) %>% summarise(N = n(),
                                                              Y = mean(Y),
                                                              cell_id = first(pop_cell_id)) %>%
  ungroup() %>% mutate(
    age = as.factor(age),
    eth = as.factor(eth),
    edu = as.factor(edu)
    )

mu_cell_true <- aggregate(. ~ pop_cell_id, data = pop_data, mean)$Y
  
R <- 3
n_r <- rep(0, R)
bias_mu_pred <- rep(0, R)
sd_mu_pred <- rep(0, R)
cr_mu_pred <- rep(0, R)
bias_mu_sample <- rep(0, R)
sd_mu_sample <- rep(0, R)
cr_mu_sample <- rep(0, R)
bias_mu_popcell <- matrix(0, R, J_true)
sd_mu_popcell <- matrix(0, R, J_true)
cr_mu_popcell <- matrix(0, R, J_true)
bias_mu_cell <- matrix(0, R, J_true)
sd_mu_cell <- matrix(0, R, J_true)
cr_mu_cell <- matrix(0, R, J_true)

bias_mu_pred_iid <- rep(0, R)
sd_mu_pred_iid <- rep(0, R)
cr_mu_pred_iid <- rep(0, R)
bias_mu_sample_iid <- rep(0, R)
sd_mu_sample_iid <- rep(0, R)
cr_mu_sample_iid <- rep(0, R)
bias_mu_popcell_iid <- matrix(0, R, J_true)
sd_mu_popcell_iid <- matrix(0, R, J_true)
cr_mu_popcell_iid <- matrix(0, R, J_true)
bias_mu_cell_iid <- matrix(0, R, J_true)
sd_mu_cell_iid <- matrix(0, R, J_true)
cr_mu_cell_iid <- matrix(0, R, J_true)

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
  
  J <- length(unique(cell_id))
  J_use <- as.numeric(names(table(cell_id)))
  n_cell <- dat_rstanarm$n
  
  N_cell <- dat_rstanarm$N
  
  mu_cell_use <- agg_pop %>% filter(cell_id %in% dat_rstanarm$cell_id) %>% .$Y
  
  ###-----------------STAN with structural prior--------------------------###
  
  S_arm <- rstanarm::stan_glmer(formula = ff, data = dat_rstanarm, iter = 2000, chains = 4, cores = 4, 
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
  bias_mu_popcell[r, ] <- apply(output$mu_cell_pred, 2, mean) - agg_pop$Y
  sd_mu_popcell[r, ] <- apply(output$mu_cell_pred, 2, sd)
  cr_mu_popcell[r, ] <- as.numeric(apply(output$mu_cell_pred, 2, quantile, 0.025) <= agg_pop$Y & agg_pop$Y <= 
      apply(output$mu_cell_pred, 2, quantile, 0.975))
  
  ### sampled cell mean
  bias_mu_cell[r, J_use] <- apply(output$mu_cell, 2, mean) - dat_rstanarm$Y
  sd_mu_cell[r, J_use] <- apply(output$mu_cell, 2, sd)
  cr_mu_cell[r, J_use] <- as.numeric(apply(output$mu_cell, 2, quantile, 0.025) <= dat_rstanarm$Y & dat_rstanarm$Y <= 
      apply(output$mu_cell, 2, quantile, 0.975))
  
  
  ### weights
  wght_sd_rt[r, ] <- c(sd(output$w_unit), max(output$w_unit)/min(output$w_unit))
  
  bias_mu_st[r] <- output$mu_w - mean(Y)
  sd_mu_st[r] <- sqrt(sum(output$w_unit^2 * var(dat$Y)))/n
  
  cr_mu_st[r] <- as.numeric(output$mu_w - 1.96 * sd_mu_st[r] <= mean(Y) & mean(Y) <= output$mu_w + 1.96 * sd_mu_st[r])
  
  ###-----------------STAN with independent prior--------------------------###
  
  
  S1 <- rstanarm::stan_glmer(formula = ff, data = dat_rstanarm, iter = 2000, chains = 4, cores = 4,
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
  bias_mu_popcell_iid[r, ] <- apply(output_iid$mu_cell_pred, 2, mean) - agg_pop$Y
  sd_mu_popcell_iid[r, ] <- apply(output_iid$mu_cell_pred, 2, sd)
  cr_mu_popcell_iid[r, ] <- as.numeric(apply(output_iid$mu_cell_pred, 2, quantile, 0.025) <= agg_pop$Y & 
                                         agg_pop$Y <= apply(output_iid$mu_cell_pred, 2, quantile, 0.975))
  
  ### sampled cell mean
  bias_mu_cell_iid[r, J_use] <- apply(output_iid$mu_cell, 2, mean) - dat_rstanarm$Y
  sd_mu_cell_iid[r, J_use] <- apply(output_iid$mu_cell, 2, sd)
  cr_mu_cell_iid[r, J_use] <- as.numeric(apply(output_iid$mu_cell, 2, quantile, 0.025) <= dat_rstanarm$Y & dat_rstanarm$Y <= 
                                           apply(output_iid$mu_cell, 2, quantile, 0.975))
  
  ### weights
  wght_sd_rt_iid[r, ] <- c(sd(output_iid$w_unit), max(output_iid$w_unit)/min(output_iid$w_unit))
  
  bias_mu_id[r] <- output_iid$mu_w - mean(Y)
  sd_mu_id[r] <- sqrt(sum(output_iid$w_unit^2 * var(dat$Y)))/n
  cr_mu_id[r] <- as.numeric(output$mu_w - 1.96 * sd_mu_id[r] <= mean(Y) & mean(Y) <= output_iid$mu_w + 1.96 * sd_mu_id[r])
  
  ###------PS weighting------###
  w_ps_df <- dat[,c('cell_id','Y')] %>%
    left_join(agg_pop[,c('cell_id','N')], by = 'cell_id') %>%
    left_join(dat_rstanarm[,c('cell_id','n')], by = 'cell_id') %>%
    mutate(w_ps = N / n,
           w_ps = w_ps / mean(w_ps),
           Y_w = w_ps * Y)
  w_ps <- w_ps_df %>% .$w_ps
  wght_sd_rt_ps[r, ] <- c(sd(w_ps), max(w_ps)/min(w_ps))

  mu_ps <- mean(w_ps_df$Y_w)
  bias_mu_ps[r] <- mu_ps - mean(Y)
  sd_mu_ps[r] <- sqrt(sum(w_ps^2 * var(dat$Y)))/n
  cr_mu_ps[r] <- as.numeric(mu_ps - 1.96 * sd_mu_ps[r] <= mean(Y) & mean(Y) <= mu_ps + 1.96 * sd_mu_ps[r])
  ###------inverse-prob weighted estimator------###
  w_ips <- 1/sel_prob[I]/mean(1/sel_prob[I])
  w_ips_df <- data.frame(w_ips = w_ips, 
                         Y = dat$Y,
                         cell_id = dat$cell_id) %>%
    mutate(
      Y_w = w_ips * Y
    )

  wght_sd_rt_ips[r, ] <- c(sd(w_ips), max(w_ips)/min(w_ips))

  mu_ips <- mean(w_ips_df$Y_w)
  bias_mu_ips[r] <- mu_ips - mean(Y)
  sd_mu_ips[r] <- sqrt(sum(w_ips^2 * var(dat$Y)))/n
  cr_mu_ips[r] <- as.numeric(mu_ips - 1.96 * sd_mu_ips[r] <= mean(Y) & mean(Y) <= mu_ips + 1.96 * sd_mu_ips[r])
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

  mu_rake <- mean(w_rake * dat$Y)
  bias_mu_rake[r] <- mu_rake - mean(Y)
  sd_mu_rake[r] <- sqrt(sum(w_rake^2 * var(dat$Y)))/n
  cr_mu_rake[r] <- as.numeric(mu_rake - 1.96 * sd_mu_rake[r] <= mean(Y) & mean(Y) <= mu_rake + 1.96 * sd_mu_rake[r])

  ### sub domain
  st_est_sm <- rep(0, dim(output$mu_cell_pred)[1])
  iid_est_sm <- rep(0, dim(output$mu_cell_pred)[1])
  
  cell_str <- agg_pop[,c('age','eth','edu')]
  
  for (v in 1:q) {
    for (l in 1:l_v[v + 1]) {
      sub_pop_data <- agg_pop[which(cell_str[,v] == l),c('cell_id','Y','N')] 
      sub_cell_idx <- sub_pop_data$cell_id
      cell_pred_idx <- match(sub_cell_idx, colnames(output$mu_cell_pred))
      mar_true <- sum(sub_pop_data$Y * sub_pop_data$N/sum(sub_pop_data$N))
      # model prediction
      st_est_sm <- output$mu_cell_pred[,cell_pred_idx] %*% (sub_pop_data$N / sum(sub_pop_data$N))
      bias_sub_st[r, l + sum(l_v[1:v])] <- mean(st_est_sm) - mar_true
      sd_sub_st[r, l + sum(l_v[1:v])] <- sd(st_est_sm)
      if (quantile(st_est_sm, 0.025) <= mar_true & mar_true <= quantile(st_est_sm, 0.975)) {
        cr_sub_st[r, l + sum(l_v[1:v])] <- 1
      }

      # independent prior
      iid_est_sm <- output_iid$mu_cell_pred[,match(sub_cell_idx, colnames(output_iid$mu_cell_pred))] %*%
                                          (sub_pop_data$N / sum(sub_pop_data$N))
      bias_sub_iid[r, l + sum(l_v[1:v])] <- mean(iid_est_sm) - mar_true
      sd_sub_iid[r, l + sum(l_v[1:v])] <- sd(iid_est_sm)
      if (quantile(iid_est_sm, 0.025) <= mar_true & mar_true <= quantile(iid_est_sm, 0.975)) {
        cr_sub_iid[r, l + sum(l_v[1:v])] <- 1
      }
      # model-based weights under st prior
      unit_idx <- st_out$cell_id %in% sub_cell_idx
      est_st_wt <- sum(st_out$Y_w[unit_idx])/sum(st_out$w_unit[unit_idx])
      bias_sub_st_wt[r, l + sum(l_v[1:v])] <- est_st_wt - mar_true
      sd_sub_st_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(st_out$w_unit[unit_idx]^2 * var(st_out$Y[unit_idx])))/sum(st_out$w_unit[unit_idx])
      cr_sub_st_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_st_wt - 1.96 * sd_sub_st_wt[r, l + sum(l_v[1:v])] <=
                                                         mar_true & mar_true <= est_st_wt + 1.96 * sd_sub_st_wt[r, l + sum(l_v[1:v])])

      # model-based weights under independent prior
      unit_idx <- st_iid$cell_id %in% sub_cell_idx
      est_iid_wt <- sum(st_iid$Y_w[unit_idx])/sum(st_iid$w_unit[unit_idx])
      bias_sub_iid_wt[r, l + sum(l_v[1:v])] <- est_iid_wt - mar_true
      sd_sub_iid_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(st_iid$w_unit[unit_idx]^2 * var(st_iid$Y[unit_idx])))/sum(st_iid$w_unit[unit_idx])
      cr_sub_iid_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_iid_wt - 1.96 * sd_sub_iid_wt[r, l + sum(l_v[1:v])] <=
                                                          mar_true & mar_true <= est_iid_wt + 1.96 * sd_sub_iid_wt[r, l + sum(l_v[1:v])])

      
      unit_idx <- w_ps_df$cell_id %in% sub_cell_idx
      # ps weights
      est_ps_wt <- sum(w_ps_df$Y_w[unit_idx])/sum(w_ps_df$w_ps[unit_idx])
      bias_sub_ps_wt[r, l + sum(l_v[1:v])] <- est_ps_wt - mar_true
      sd_sub_ps_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(w_ps_df$w_ps[unit_idx]^2 * var(dat$Y[unit_idx])))/sum(w_ps_df$w_ps[unit_idx])
      cr_sub_ps_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_ps_wt - 1.96 * sd_sub_ps_wt[r, l + sum(l_v[1:v])] <=
                                                         mar_true & mar_true <= est_ps_wt + 1.96 * sd_sub_ps_wt[r, l + sum(l_v[1:v])])
      unit_idx <- w_ips_df$cell_id %in% sub_cell_idx
      # ips weights
      est_ips_wt <- sum(w_ips_df$Y_w[unit_idx])/sum(w_ips_df$w_ips[unit_idx])
      bias_sub_ips_wt[r, l + sum(l_v[1:v])] <- est_ips_wt - mar_true
      sd_sub_ips_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(w_ips_df$w_ips[unit_idx]^2 * var(w_ips_df$Y[unit_idx])))/sum(w_ips_df$w_ips[unit_idx])
      cr_sub_ips_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_ips_wt - 1.96 * sd_sub_ips_wt[r, l + sum(l_v[1:v])] <=
                                                          mar_true & mar_true <= est_ips_wt + 1.96 * sd_sub_ips_wt[r, l + sum(l_v[1:v])])

      unit_idx <- w_rake_df$cell_id %in% sub_cell_idx
      # rake weights
      est_rake_wt <- sum(w_rake_df$Y_w[unit_idx])/sum(w_rake_df$w_rake[unit_idx])
      bias_sub_rake_wt[r, l + sum(l_v[1:v])] <- est_rake_wt - mar_true
      sd_sub_rake_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(w_rake_df$w_rake[unit_idx]^2 * var(w_rake_df$Y[unit_idx])))/sum(w_rake_df$w_rake[unit_idx])
      cr_sub_rake_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_rake_wt - 1.96 * sd_sub_rake_wt[r, l + sum(l_v[1:v])] <=
                                                           mar_true & mar_true <= est_rake_wt + 1.96 * sd_sub_rake_wt[r, l + sum(l_v[1:v])])

    }
  }
  # interaction
  sub_pop_data <- agg_pop[which(cell_str[, 1] == 1 & cell_str[, 2] != 1),c('cell_id','Y','N')] 
  sub_cell_idx <- sub_pop_data$cell_id
  cell_pred_idx <- match(sub_cell_idx, colnames(output$mu_cell_pred))
  mar_true <- sum(sub_pop_data$Y * sub_pop_data$N)/sum(sub_pop_data$N)

  # model prediction
  st_est_sm <- output$mu_cell_pred[,cell_pred_idx] %*% (sub_pop_data$N / sum(sub_pop_data$N))
  bias_sub_st_int[r] <- mean(st_est_sm) - mar_true
  sd_sub_st_int[r] <- sd(st_est_sm)
  if (quantile(st_est_sm, 0.025) <= mar_true & mar_true <= quantile(st_est_sm, 0.975)) {
    cr_sub_st_int[r] <- 1
  }

  # independent prior
  iid_est_sm <- output_iid$mu_cell_pred[,match(sub_cell_idx, colnames(output_iid$mu_cell_pred))] %*% 
                (sub_pop_data$N / sum(sub_pop_data$N))
  bias_sub_iid_int[r] <- mean(iid_est_sm) - mar_true
  sd_sub_iid_int[r] <- sd(iid_est_sm)
  if (quantile(iid_est_sm, 0.025) <= mar_true & mar_true <= quantile(iid_est_sm, 0.975)) {
    cr_sub_iid_int[r] <- 1
  }

  # model-based weights under st prior
  unit_idx <- st_out$cell_id %in% sub_cell_idx
  est_st_wt <- sum(st_out$Y_w[unit_idx])/sum(st_out$w_unit[unit_idx])
  bias_sub_st_wt_int[r] <- est_st_wt - mar_true
  sd_sub_st_wt_int[r] <- sqrt(sum(st_out$w_unit[unit_idx]^2 * var(st_out$Y[unit_idx])))/sum(st_out$w_unit[unit_idx])
  cr_sub_st_wt_int[r] <- as.numeric(est_st_wt - 1.96 * sd_sub_st_wt_int[r] <= mar_true & mar_true <= est_st_wt +
                                      1.96 * sd_sub_st_wt_int[r])

  unit_idx <- st_iid$cell_id %in% sub_cell_idx
  # model-based weights under independent prior
  est_iid_wt <- sum(st_iid$Y_w)/sum(st_iid$w_unit[unit_idx])
  bias_sub_iid_wt_int[r] <- est_iid_wt - mar_true
  sd_sub_iid_wt_int[r] <- sqrt(sum(st_iid$w_unit[unit_idx]^2 * var(st_iid$Y[unit_idx])))/sum(st_iid$w_unit[unit_idx])
  cr_sub_iid_wt_int[r] <- as.numeric(est_iid_wt - 1.96 * sd_sub_iid_wt_int[r] <= mar_true & mar_true <= est_iid_wt +
                                       1.96 * sd_sub_iid_wt_int[r])

  # ps weights
  unit_idx <- w_ps_df$cell_id %in% sub_cell_idx
  est_ps_wt <- sum(w_ps_df$Y_w)/sum(w_ps_df$w_ps[unit_idx])
  bias_sub_ps_wt_int[r] <- est_ps_wt - mar_true
  sd_sub_ps_wt_int[r] <- sqrt(sum(w_ps_df$w_ps[unit_idx]^2 * var(w_ps_df$Y[unit_idx])))/sum(w_ps_df$w_ps[unit_idx])
  cr_sub_ps_wt_int[r] <- as.numeric(est_ps_wt - 1.96 * sd_sub_ps_wt_int[r] <= mar_true & mar_true <= est_ps_wt +
                                      1.96 * sd_sub_ps_wt_int[r])
  # ips weights
  unit_idx <- w_ips_df$cell_id %in% sub_cell_idx
  est_ips_wt <- sum(w_ips_df$Y_w[unit_idx])/sum(w_ips_df$w_ips[unit_idx])
  bias_sub_ips_wt_int[r] <- est_ips_wt - mar_true
  sd_sub_ips_wt_int[r] <- sqrt(sum(w_ips_df$w_ips[unit_idx]^2 * var(w_ips_df$Y[unit_idx])))/sum(w_ips_df$w_ips[unit_idx])
  cr_sub_ips_wt_int[r] <- as.numeric(est_ips_wt - 1.96 * sd_sub_ips_wt_int[r] <= mar_true & mar_true <= est_ips_wt +
                                       1.96 * sd_sub_ips_wt_int[r])

  # rake weights
  unit_idx <- w_rake_df$cell_id %in% sub_cell_idx
  est_rake_wt <- sum(w_rake_df$Y_w[unit_idx])/sum(w_rake_df$w_rake[unit_idx])
  bias_sub_rake_wt_int[r] <- est_rake_wt - mar_true
  sd_sub_rake_wt_int[r] <- sqrt(sum(w_rake_df$w_rake[unit_idx]^2 * var(w_rake_df$Y[unit_idx])))/sum(w_rake_df$w_rake[unit_idx])
  cr_sub_rake_wt_int[r] <- as.numeric(est_rake_wt - 1.96 * sd_sub_rake_wt_int[r] <= mar_true & mar_true <= est_rake_wt +
                                        1.96 * sd_sub_rake_wt_int[r])
     
}  #end of repeated sampling
