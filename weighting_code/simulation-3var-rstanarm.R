###---------------MRP to Construct Survey Weights--------------###
### Author: YS & RT & JG Latest Edit date: 07/19/2017 clear
remove(list = objects())

#----------required packages-----------------#
library(rstanarm)
library(survey)
library(ggplot2)
library(dplyr)
library(directlabels)

set.seed(20150213)

source('helper_functions.R')

# load clean population data
acs_ad <- readRDS('data/acs_ad.RDS')

q <- 3  # number of weighting variables

N <- nrow(acs_ad)
J_age <- length(unique(acs_ad$age))
J_eth <- length(unique(acs_ad$eth))
J_edu <- length(unique(acs_ad$edu))

options("contrasts")
age <- model.matrix( ~ age, acs_ad)
eth <- model.matrix( ~ eth, acs_ad)
edu <- model.matrix( ~ edu, acs_ad)
age_eth <- model.matrix(~ 0 + age:eth, acs_ad)
age_edu <- model.matrix(~ 0 + age:edu, acs_ad)
eth_edu <- model.matrix(~ 0 + eth:edu, acs_ad)
age_eth_edu <- model.matrix(~ 0 + age:eth:edu, acs_ad)

### simulate Y

col_vec <- function(x, nr) matrix(x, nrow = nr, ncol = 1)

betaY_age <- col_vec(seq(0.5, 4, length = J_age), J_age)
betaY_eth <- col_vec(seq(-2, 2, length = J_eth), J_eth)
betaY_edu <- col_vec(seq(3, 0, length = J_edu), J_edu)
betaY_age_eth <- col_vec(sample(1:4, J_age * J_eth, replace = T), J_age * J_eth)
betaY_age_edu <- col_vec(sample(-2:2, J_age * J_edu, replace = T), J_age * J_edu)
betaY_eth_edu <- col_vec(sample(-3:1, J_eth * J_edu, replace = T), J_eth * J_edu)
betaY_age_eth_edu <- col_vec(sample(seq(-1, 1, by = 0.5), J_age * J_eth * J_edu, replace = T), J_age * J_eth * J_edu)

muY <-
  age %*% betaY_age + 
  eth %*% betaY_eth + 
  edu %*% betaY_edu + 
  age_eth %*% betaY_age_eth + 
  age_edu %*% betaY_age_edu + 
  eth_edu %*% betaY_eth_edu + 
  age_eth_edu %*% betaY_age_eth_edu

y <- rnorm(N) + muY

betaI_age <- col_vec(seq(-2, 0, length = J_age), J_age)
betaI_eth <- col_vec(seq(-1, 1, length = J_eth), J_eth)
betaI_edu <- col_vec(seq(0, 3, length = J_edu), J_edu)
betaI_age_eth <- col_vec(0, J_age * J_eth)
betaI_age_edu <- col_vec(0, J_age * J_edu)
betaI_eth_edu <- col_vec(0, J_eth * J_edu)
betaI_age_eth_edu <- col_vec(0, J_age * J_eth * J_edu)

sel_prob <-
  plogis(
    -2 + 
      age %*% betaI_age + 
      eth %*% betaI_eth + 
      edu %*% betaI_edu + 
      age_eth %*% betaI_age_eth +
      age_edu %*% betaI_age_edu + 
      eth_edu %*% betaI_eth_edu + 
      age_eth_edu %*% betaI_age_eth_edu
  )

pop_data <- 
  acs_ad %>%
  select(age, eth, edu) %>%
  mutate(pop_cell_id = paste0(age, eth, edu), 
         Y = as.vector(y))

pop_cell_id <- pop_data$pop_cell_id
pstrat_cell_ids <- 
  expand.grid(age = 1:J_age, eth = 1:J_eth, edu = 1:J_edu) %>%
  mutate(cell_id = paste0(age, eth, edu)) %>% 
  .$cell_id
J_true <- length(pstrat_cell_ids)

# Aggregates population data by poststratification cell
agg_pop <- 
  pop_data %>% 
  group_by(age, eth, edu) %>%
  summarise(
    N = n(),
    Y = mean(Y),
    cell_id = first(pop_cell_id)
  ) %>%
  ungroup()

R <- 500 # number of repeats
n_r <-
  bias_mu_pred <-
  sd_mu_pred <-
  cr_mu_pred <-
  bias_mu_sample <-
  sd_mu_sample <-
  cr_mu_sample <- rep(0, R)

bias_mu_popcell <-
  sd_mu_popcell <-
  cr_mu_popcell <-
  bias_mu_cell <-
  sd_mu_cell <-
  cr_mu_cell <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))

bias_mu_pred_iid <-
  sd_mu_pred_iid <-
  cr_mu_pred_iid <-
  bias_mu_sample_iid <-
  sd_mu_sample_iid <-
  cr_mu_sample_iid <- rep(0, R)

bias_mu_popcell_iid <-
  sd_mu_popcell_iid <-
  cr_mu_popcell_iid <-
  bias_mu_cell_iid <-
  sd_mu_cell_iid <-
  cr_mu_cell_iid <- matrix(0, R, J_true, dimnames = list(NULL, pstrat_cell_ids))

bias_mu_st <-
  sd_mu_st <-
  cr_mu_st <-
  bias_mu_id <-
  sd_mu_id <-
  cr_mu_id <-
  bias_mu_ps <-
  sd_mu_ps <-
  cr_mu_ps <-
  bias_mu_ips <-
  sd_mu_ips <-
  cr_mu_ips <-
  bias_mu_rake <-
  sd_mu_rake <-
  cr_mu_rake <- rep(0, R)

wght_sd_rt <-
  wght_sd_rt_ps <-
  wght_sd_rt_ips <-
  wght_sd_rt_rake <-
  wght_sd_rt_iid <- matrix(0, R, 2)

l_v <- c(0, J_age, J_eth, J_edu)
bias_sub_st <-
  sd_sub_st <-
  cr_sub_st <-
  bias_sub_st_wt <-
  sd_sub_st_wt <-
  cr_sub_st_wt <-
  bias_sub_iid <-
  sd_sub_iid <-
  cr_sub_iid <-
  bias_sub_iid_wt <-
  sd_sub_iid_wt <-
  cr_sub_iid_wt <-
  bias_sub_ps_wt <-
  sd_sub_ps_wt <-
  cr_sub_ps_wt <-
  bias_sub_ips_wt <-
  sd_sub_ips_wt <-
  cr_sub_ips_wt <-
  bias_sub_rake_wt <-
  sd_sub_rake_wt <-
  cr_sub_rake_wt <- matrix(0, R, sum(l_v))

bias_sub_st_int <-
  sd_sub_st_int <-
  cr_sub_st_int <-
  bias_sub_st_wt_int <-
  sd_sub_st_wt_int <-
  cr_sub_st_wt_int <-
  bias_sub_iid_int <-
  sd_sub_iid_int <-
  cr_sub_iid_int <-
  bias_sub_iid_wt_int <-
  sd_sub_iid_wt_int <-
  cr_sub_iid_wt_int <-
  bias_sub_ps_wt_int <-
  sd_sub_ps_wt_int <-
  cr_sub_ps_wt_int <-
  bias_sub_ips_wt_int <-
  sd_sub_ips_wt_int <-
  cr_sub_ips_wt_int <-
  bias_sub_rake_wt_int <-
  sd_sub_rake_wt_int <-
  cr_sub_rake_wt_int <- rep(0, R)


# model formula
ff <- as.formula(Y ~ 1 + (1 | age) + (1 | eth) + (1 | edu) + 
                   (1 | age:eth) + (1 | eth:edu) + (1 | age:edu) + 
                   (1 | age:edu:eth))

# repeated sampling
for (r in 1:R) {
  set.seed(20150213 + r)
  
  ### sampling
  I <- (runif(N) <= sel_prob)
  
  dat <- 
    acs_ad[I,, drop=FALSE] %>% 
    select(age, eth, edu) %>%
    mutate(
      Y = y[I], 
      cell_id = pop_cell_id[I]
    )
    
  dat_rstanarm <-
    dat %>%
    group_by(age, eth, edu) %>%
    summarise(
      sd_cell = if_else(is.na(sd(Y)), 0, sd(Y)),
      Y = mean(Y),
      n = n(),
      cell_id = first(cell_id)
    ) %>%
    ungroup() %>%
    left_join(agg_pop[, c('cell_id', 'N')], by = 'cell_id')
  
  #-----------computation--------------#
  n <- nrow(dat)
  cell_id <- dat_rstanarm$cell_id
  
  ###-----------------STAN with structural prior--------------------------###
  
  S_arm <-
    stan_glmer(
      formula = ff,
      data = dat_rstanarm,
      iter = 500,
      chains = 4,
      cores = 4,
      prior_covariance = 
        mrp_structured(
          cell_size = dat_rstanarm$n, 
          cell_sd = dat_rstanarm$sd_cell,
          group_level_scale = 1, 
          group_level_df = 1
        ),
      seed = 123,
      prior_aux = cauchy(0, 5),
      prior_intercept = normal(0, 100, autoscale = FALSE),
      adapt_delta = 0.99
    )
  output <- sum_svey_model(S_arm, agg_pop)
  st_out <- 
    dat %>%
    left_join(output$mean_w_new, by = 'cell_id') %>% 
    mutate(
      w = w_unit / mean(w_unit),
      Y_w = w * Y
    ) %>% 
    select(cell_id, w, Y_w, Y)
  output$mu_w <- mean(st_out$Y_w)
  output$w_unit <- st_out$w
  
  # prediction also using empty cells
  check_interval <- function(x, val) {
    quantile(x, 0.025) <= val & val <= quantile(x, 0.975)
  }
  bias_mu_pred[r] <- mean(output$theta_pred) - mean(y)
  sd_mu_pred[r] <- sd(output$theta_pred)
  if (check_interval(output$theta_pred, mean(y)))
    cr_mu_pred[r] <- 1
  
  # prediction using only nonempty cells
  bias_mu_sample[r] <- mean(output$theta_sample) - mean(y)
  sd_mu_sample[r] <- sd(output$theta_sample)
  if (check_interval(output$theta_sample, mean(y)))
    cr_mu_sample[r] <- 1
  
  ### population cell mean
  bias_mu_popcell[r, colnames(output$mu_cell_pred)] <- 
    colMeans(output$mu_cell_pred) - agg_pop$Y
  sd_mu_popcell[r, colnames(output$mu_cell_pred)] <- 
    apply(output$mu_cell_pred, 2, sd)
  cr_mu_popcell[r, colnames(output$mu_cell_pred)] <-
    as.numeric(
      apply(output$mu_cell_pred, 2, quantile, 0.025) <= agg_pop$Y &
        agg_pop$Y <=
        apply(output$mu_cell_pred, 2, quantile, 0.975)
    )
  
  ### sampled cell mean
  bias_mu_cell[r, colnames(output$mu_cell)] <- 
    colMeans(output$mu_cell) - dat_rstanarm$Y
  sd_mu_cell[r, colnames(output$mu_cell)] <- 
    apply(output$mu_cell, 2, sd)
  cr_mu_cell[r, colnames(output$mu_cell)] <-
    as.numeric(
      apply(output$mu_cell, 2, quantile, 0.025) <= dat_rstanarm$Y &
        dat_rstanarm$Y <=
        apply(output$mu_cell, 2, quantile, 0.975)
    )
  
  ### weights
  wght_sd_rt[r, ] <- c(sd(output$w_unit), max(output$w_unit)/min(output$w_unit))
  w_sum <- sum_weights(weight_df = st_out, idx = st_out$cell_id, comp_stat = mean(y))
  bias_mu_st[r] <- w_sum$bias
  sd_mu_st[r] <- w_sum$sd_wt
  cr_mu_st[r] <- w_sum$cr_wt
  
  
  ###-----------------STAN with independent prior--------------------------###

  S_iid <-
    stan_glmer(
      formula = ff,
      data = dat_rstanarm,
      iter = 500,
      chains = 4,
      cores = 4,
      seed = 123,
      prior_covariance = mrp_structured(
        indep = TRUE,
        cell_size = dat_rstanarm$n,
        cell_sd = dat_rstanarm$sd_cell
      ),
      prior_aux = cauchy(0, 5), 
      adapt_delta = 0.99
    )
  output_iid <- sum_svey_model(object = S_iid, agg_pop = agg_pop)
  st_iid <- 
    dat %>%
    left_join(output_iid$mean_w_new, by = 'cell_id') %>% 
    mutate(
      w = w_unit / mean(w_unit),
      Y_w = w * Y
    ) %>% 
    select(cell_id, w, Y_w, Y)
  output_iid$mu_w <- mean(st_iid$Y_w)
  output_iid$w_unit <- st_iid$w
 
  ###-------------------------------------###
  
  # prediction also using zero cells
  bias_mu_pred_iid[r] <- mean(output_iid$theta_pred) - mean(y)
  sd_mu_pred_iid[r] <- sd(output_iid$theta_pred)
  if (check_interval(output_iid$theta_pred, mean(y)))
    cr_mu_pred_iid[r] <- 1

  # prediction using only nonzero cells
  bias_mu_sample_iid[r] <- mean(output_iid$theta_sample) - mean(y)
  sd_mu_sample_iid[r] <- sd(output_iid$theta_sample)
  if (check_interval(output_iid$theta_sample, mean(y)))
    cr_mu_sample_iid[r] <- 1
  
  ### population cell mean
  bias_mu_popcell_iid[r, colnames(output_iid$mu_cell_pred)] <- 
    colMeans(output_iid$mu_cell_pred) - agg_pop$Y
  sd_mu_popcell_iid[r, colnames(output_iid$mu_cell_pred)] <- 
    apply(output_iid$mu_cell_pred, 2, sd)
  cr_mu_popcell_iid[r, colnames(output_iid$mu_cell_pred)] <-
    as.numeric(
      apply(output_iid$mu_cell_pred, 2, quantile, 0.025) <= agg_pop$Y &
        agg_pop$Y <= apply(output_iid$mu_cell_pred, 2, quantile, 0.975)
    )
  
  ### sampled cell mean
  bias_mu_cell_iid[r, colnames(output_iid$mu_cell)] <- 
    colMeans(output_iid$mu_cell) - dat_rstanarm$Y
  sd_mu_cell_iid[r, colnames(output_iid$mu_cell)] <- 
    apply(output_iid$mu_cell, 2, sd)
  cr_mu_cell_iid[r, colnames(output_iid$mu_cell)] <-
    as.numeric(
      apply(output_iid$mu_cell, 2, quantile, 0.025) <= dat_rstanarm$Y &
        dat_rstanarm$Y <= apply(output_iid$mu_cell, 2, quantile, 0.975)
    )
  
  ### weights
  wght_sd_rt_iid[r, ] <- c(sd(output_iid$w_unit), max(output_iid$w_unit)/min(output_iid$w_unit))
  w_sum <- sum_weights(weight_df = st_iid, idx = st_iid$cell_id, comp_stat = mean(y))
  bias_mu_id[r] <- w_sum$bias
  sd_mu_id[r] <- w_sum$sd_wt
  cr_mu_id[r] <- w_sum$cr_wt
  
  ###------PS weighting------###
  w_ps_df <- 
    dat[, c('cell_id','Y')] %>%
    left_join(agg_pop[,c('cell_id','N')], by = 'cell_id') %>%
    left_join(dat_rstanarm[,c('cell_id','n')], by = 'cell_id') %>%
    mutate(
      w_ps = N / n,
      w = w_ps / mean(w_ps),
      Y_w = w * Y
    ) %>%
    select(cell_id, w, Y_w, Y)
  w_ps <- w_ps_df %>% .$w
  wght_sd_rt_ps[r,] <- c(sd(w_ps), max(w_ps) / min(w_ps))

  w_sum <- sum_weights(weight_df = w_ps_df, idx = w_ps_df$cell_id, comp_stat = mean(y))
  bias_mu_ps[r] <- w_sum$bias
  sd_mu_ps[r] <- w_sum$sd_wt
  cr_mu_ps[r] <- w_sum$cr_wt
  
  ###------inverse-prob weighted estimator------###
  w_ips <- 1 / sel_prob[I] / mean(1 / sel_prob[I])
  w_ips_df <- 
    dat %>% 
    select(Y, cell_id) %>%
    mutate(
      w = w_ips, 
      Y_w = w * Y
    ) %>%
    select(cell_id, w, Y_w, Y)

  wght_sd_rt_ips[r, ] <- c(sd(w_ips), max(w_ips)/min(w_ips))
  w_sum <- sum_weights(weight_df = w_ips_df, idx = w_ips_df$cell_id, comp_stat = mean(y))
  bias_mu_ips[r] <- w_sum$bias
  sd_mu_ips[r] <- w_sum$sd_wt
  cr_mu_ips[r] <- w_sum$cr_wt
  
  ###------raking estimator------###
  dat.design <- svydesign(id = ~ 1, data = dat)

  pop.age <- data.frame(age = 1:J_age, Freq = as.numeric(table(acs_ad$age)))
  pop.eth <- data.frame(eth = 1:J_eth, Freq = as.numeric(table(acs_ad$eth)))
  pop.edu <- data.frame(edu = 1:J_edu, Freq = as.numeric(table(acs_ad$edu)))
  dat_rake <-
    rake(dat.design,
         list( ~ age, ~ eth, ~ edu),
         list(pop.age, pop.eth, pop.edu))

  w_rake <- weights(dat_rake) / mean(weights(dat_rake))
  w_rake_df <- 
    dat %>%
    select(Y, cell_id) %>% 
    mutate(
      w = w_rake, 
      Y_w = w * Y
    ) %>%
    select(cell_id, w, Y_w, Y)

  wght_sd_rt_rake[r, ] <- c(sd(w_rake), max(w_rake)/min(w_rake))
  w_sum <- sum_weights(weight_df = w_rake_df, idx = w_rake_df$cell_id, comp_stat = mean(y))
  bias_mu_rake[r] <- w_sum$bias
  sd_mu_rake[r] <- w_sum$sd_wt
  cr_mu_rake[r] <- w_sum$cr_wt

  ### sub domain
  cell_str <- agg_pop[,c('age','eth','edu')]
  
  for (v in 1:q) {
    for (l in 1:l_v[v + 1]) {
      sub_pop_data <- agg_pop[which(cell_str[, v] == l), c('cell_id','Y','N')] 
      sub_cell_idx <- sub_pop_data$cell_id
      mar_true <- sum(sub_pop_data$Y * sub_pop_data$N/sum(sub_pop_data$N))
      # model prediction
      st_est_sm <- output$mu_cell_pred[,sub_cell_idx] %*% (sub_pop_data$N / sum(sub_pop_data$N))
      bias_sub_st[r, l + sum(l_v[1:v])] <- mean(st_est_sm) - mar_true
      sd_sub_st[r, l + sum(l_v[1:v])] <- sd(st_est_sm)
      if (check_interval(st_est_sm, mar_true)) 
        cr_sub_st[r, l + sum(l_v[1:v])] <- 1

      # independent prior
      iid_est_sm <- output_iid$mu_cell_pred[,sub_cell_idx] %*% (sub_pop_data$N / sum(sub_pop_data$N))
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
  if (check_interval(st_est_sm, mar_true))
    cr_sub_st_int[r] <- 1

  # independent prior
  iid_est_sm <- output_iid$mu_cell_pred[,sub_cell_idx] %*% 
                (sub_pop_data$N / sum(sub_pop_data$N))
  bias_sub_iid_int[r] <- mean(iid_est_sm) - mar_true
  sd_sub_iid_int[r] <- sd(iid_est_sm)
  if (check_interval(iid_est_sm, mar_true))
    cr_sub_iid_int[r] <- 1

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

###------------plots------------### 

plotdata <- data.frame(
  quant = factor(
    rep(
      c(
        "overall",
        "non-white young",
        "age:18-34",
        "age:35-44",
        "age:45-54",
        "age:55-64",
        "age:65+",
        "white&non-Hisp",
        "black&non-Hisp",
        "Asian",
        "Hisp",
        "other race/eth",
        "<high sch",
        "high sch",
        "some col",
        ">=col"
      ),
      times = 4 * 7
    ),
    levels = c(
      "age:18-34",
      "age:35-44",
      "age:45-54",
      "age:55-64",
      "age:65+",
      "white&non-Hisp",
      "black&non-Hisp",
      "Asian",
      "Hisp",
      "other race/eth",
      "<high sch",
      "high sch",
      "some col",
      ">=col",
      "non-white young",
      "overall"
    )
  ), 
  method = factor(
    rep(c("Str.P", "Ind.P", "Str.W", "Ind.W", "PS.W", "Rake.W", "IP.W"), 
        each = 16 * 4),
    levels = c("Str.P", "Ind.P", "Str.W", "Ind.W", "PS.W", "Rake.W", "IP.W")
  ),
  est = factor(
    rep(rep(c("bias", "rmse", "avg_sd", "coverage"), each = 16), 
        times = 7),
    levels = c("bias", "rmse", "avg_sd", "coverage")
    ), 
  value = c(
    # Str.P
    abs(colMeans(cbind(bias_mu_pred, bias_sub_st_int, bias_sub_st))),
    sqrt(colMeans(cbind(bias_mu_pred^2, bias_sub_st_int^2, bias_sub_st^2))),
    colMeans(cbind(sd_mu_pred, sd_sub_st_int, sd_sub_st)),
    colMeans(cbind(cr_mu_pred, cr_sub_st_int, cr_sub_st)),
    # Ind.P
    abs(colMeans(cbind(bias_mu_pred_iid, bias_sub_iid_int, bias_sub_iid))),
    sqrt(colMeans(cbind(bias_mu_pred_iid ^ 2, bias_sub_iid_int ^ 2, bias_sub_iid ^ 2))),
    colMeans(cbind(sd_mu_pred_iid, sd_sub_iid_int, sd_sub_iid)),
    colMeans(cbind(cr_mu_pred_iid, cr_sub_iid_int, cr_sub_iid)),
    # Str.W
    abs(colMeans(cbind(bias_mu_st, bias_sub_st_wt_int, bias_sub_st_wt))),
    sqrt(colMeans(cbind(bias_mu_st ^ 2, bias_sub_st_wt_int ^ 2, bias_sub_st_wt ^ 2))),
    colMeans(cbind(sd_mu_st, sd_sub_st_wt_int, sd_sub_st_wt)),
    colMeans(cbind(cr_mu_st, cr_sub_st_wt_int, cr_sub_st_wt)),
    # Ind.W
    abs(colMeans(cbind(bias_mu_id, bias_sub_iid_wt_int, bias_sub_iid_wt))),
    sqrt(colMeans(cbind(bias_mu_id ^ 2, bias_sub_iid_wt_int ^ 2, bias_sub_iid_wt ^ 2))),
    colMeans(cbind(sd_mu_id, sd_sub_iid_wt_int, sd_sub_iid_wt)),
    colMeans(cbind(cr_mu_id, cr_sub_iid_wt_int, cr_sub_iid_wt)),
    # PS.W
    abs(colMeans(cbind(bias_mu_ps, bias_sub_ps_wt_int, bias_sub_ps_wt))),
    sqrt(colMeans(cbind(bias_mu_ps ^ 2, bias_sub_ps_wt_int ^ 2, bias_sub_ps_wt ^ 2))),
    colMeans(cbind(sd_mu_ps, sd_sub_ps_wt_int, sd_sub_ps_wt)),
    colMeans(cbind(cr_mu_ps, cr_sub_ps_wt_int, cr_sub_ps_wt)),
    # Rake.W
    abs(colMeans(cbind(bias_mu_rake, bias_sub_rake_wt_int, bias_sub_rake_wt))),
    sqrt(colMeans(cbind(bias_mu_rake ^ 2, bias_sub_rake_wt_int ^ 2, bias_sub_rake_wt ^ 2))),
    colMeans(cbind(sd_mu_rake, sd_sub_rake_wt_int, sd_sub_rake_wt)),
    colMeans(cbind(cr_mu_rake, cr_sub_rake_wt_int, cr_sub_rake_wt)),
    # IP.W
    abs(colMeans(cbind(bias_mu_ips, bias_sub_ips_wt_int, bias_sub_ips_wt))),
    sqrt(colMeans(cbind(bias_mu_ips ^ 2, bias_sub_ips_wt_int ^ 2, bias_sub_ips_wt ^ 2))),
    colMeans(cbind(sd_mu_ips, sd_sub_ips_wt_int, sd_sub_ips_wt)),
    colMeans(cbind(cr_mu_ips, cr_sub_ips_wt_int, cr_sub_ips_wt))
  )
)

theme_set(bayesplot::theme_default(base_family = "sans"))
plotdata %>% 
  filter(est == "bias") %>%
  ggplot(aes(method, quant)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(name = "abs(Bias)", low = "white", high = "steelblue") +
  labs(x = "", y = "") +
  theme(axis.text = element_text(size = 20))
ggsave("plot/var3_bias.pdf")

plotdata %>% 
  filter(est == "rmse") %>%
  ggplot(aes(method, quant)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(name = "RMSE", low = "white", high = "steelblue") +
  labs(x = "", y = "") +
     theme(axis.text = element_text(size = 20))
ggsave("plot/var3_rmse.pdf")


plotdata %>% 
  filter(est == "avg_sd") %>%
  ggplot(aes(method, quant)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(name = "Avg SD", low = "white", high = "steelblue") +
  labs(x = "", y = "") +
     theme(axis.text = element_text(size = 20))
ggsave("plot/var3_se.pdf")



plotdata %>% 
  filter(est == "coverage") %>%
  ggplot(aes(method, quant)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(name = "Coverage", low = "steelblue", high = "white") +
  labs(x = "", y = "") +
     theme(axis.text = element_text(size = 20))
ggsave("plot/var3_cr.pdf")


