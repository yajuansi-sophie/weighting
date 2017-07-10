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
  ret_list$theta_sample <- ret_list$mu_cell %*% (cell_table$N / sum(cell_table$N))
  ret_list$theta_pred <- ret_list$mu_cell_pred %*% (agg_pop$N / sum(agg_pop$N))
  ret_list$mean_w_new <- data.frame(w_unit = colMeans(ret_list$w_new), 
                                    id_cell = model_data$id_cell)
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

pop_cell_id <- rep(0, N)
cell_str <- matrix(0, J_age * J_eth * J_edu, q)
j <- 0
for (i3 in 1:J_edu) {
    for (i2 in 1:J_eth) {
        for (i1 in 1:J_age) {
            j <- (i3 - 1) * J_eth * J_age + (i2 - 1) * J_age + i1
            pop_cell_id[acs_ad$age_dc == i1 & acs_ad$race_dc == i2 & acs_ad$educat == i3] <- j
            cell_str[j, ] <- c(i1, i2, i3)
        }
    }
}
J_true <- J_age * J_eth * J_edu
N_cell_true <- as.numeric(table(pop_cell_id))

pop_data <- data.frame(age = acs_ad$age_dc, eth = acs_ad$race_dc, edu = acs_ad$educat, pop_cell_id, Y = Y)

# Aggregates population data by poststratification cell
agg_pop <- pop_data %>% group_by(age, eth, edu) %>% summarise(N = n(),
                                                              Y = mean(Y),
                                                              pop_cell_id = first(pop_cell_id)) %>%
  ungroup() %>% mutate(
    age = as.factor(age),
    eth = as.factor(eth),
    edu = as.factor(edu),
    id_cell = paste0(age,eth,edu)
    )

mu_cell_true <- aggregate(. ~ pop_cell_id, data = pop_data, mean)$Y
###------compiling------###
I <- (runif(N) <= sel_prob)

dat <- data.frame(Y = Y[I], age = acs_ad$age_dc[I], eth = acs_ad$race_dc[I], edu = acs_ad$educat[I])
#-----------computation--------------#
### cell id
n <- dim(dat)[1]  #sample size

cell_id <- pop_cell_id[I]

J <- length(unique(cell_id))
J_use <- as.numeric(names(table(cell_id)))
n_cell <- as.numeric(table(cell_id))

N_cell <- as.numeric(table(pop_cell_id))[as.numeric(names(table(cell_id)))]

mu_cell_use <- mu_cell_true[J_use]

dat %>% 
  group_by(age, eth, edu) %>%
  summarise(
    sq = sum((Y - mean(Y))^2)
  ) %>% ungroup() %>%
  summarise(
    ss_cell = sum(sq)
  ) %>% .$ss_cell -> ss_cell
  
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
  
  dat <- data.frame(Y = Y[I], age = acs_ad$age_dc[I], eth = acs_ad$race_dc[I], edu = acs_ad$educat[I]) %>%
    mutate(
      id_cell = paste0(age,eth,edu)
    )
  dat %>% group_by(age, eth, edu) %>% summarise(Y = mean(Y), n = n()) -> dat_rstanarm
  dat_rstanarm %>% ungroup() %>% 
    mutate(
      id_cell = paste0(age,eth,edu),
      age = as.factor(age),
      edu = as.factor(edu),
      eth = as.factor(eth)
    ) %>% 
    left_join(
      agg_pop[,c('id_cell','N')], 
    by = 'id_cell'
  ) -> dat_rstanarm
  #-----------computation--------------#
  ### cell id
  n <- dim(dat)[1]  #sample size
  n_r[r] <- n
  cell_id <- pop_cell_id[I]
  
  J <- length(unique(cell_id))
  J_use <- as.numeric(names(table(cell_id)))
  n_cell <- as.numeric(table(cell_id))
  
  N_cell <- as.numeric(table(pop_cell_id))[as.numeric(names(table(cell_id)))]
  
  mu_cell_use <- mu_cell_true[J_use]
  
  dat %>% rename(Y_indiv = Y) %>% 
    left_join(dat_rstanarm, by = 'id_cell') %>%
    mutate(dev_y = Y_indiv - Y) %>% 
    ungroup() %>% summarise(ss_cell = sum(dev_y^2)) %>% .$ss_cell -> ss_cell
  # ss_cell <- 0
  # for (j in 1:J) {
  #     ss_cell <- ss_cell + sum((dat$Y[cell_id == J_use[j]] - y_cell[j])^2)
  # }
  
  ###-----------------STAN with structural prior--------------------------###
  
  stan.seed <- round(runif(1, 0, 9999999))
  S_arm <- rstanarm::stan_glmer(formula = ff, data = dat_rstanarm, iter = 2000, chains = 4, cores = 4, 
                            prior_covariance = rstanarm::mrp_structured(scale_weights = 1/sqrt(dat_rstanarm$n)), 
                            seed = 123, prior_aux = normal(1,0.5))
  output <- sum_svey_model(S_arm, agg_pop)
  dat %>%
    left_join(
      output$mean_w_new, by = 'id_cell'
  ) %>% mutate(
    w_unit = w_unit / mean(w_unit),
    Y_w = w_unit * Y
  ) %>% select(w_unit, Y_w) -> st_out
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
                             prior_covariance = rstanarm::mrp_structured(indep=TRUE, scale_weights = 1/sqrt(dat_rstanarm$n)), 
                             prior_aux = normal(1,0.5))
  output_iid <- sum_svey_model(object = S1, agg_pop = agg_pop)
  dat %>%
    left_join(
      output_iid$mean_w_new, by = 'id_cell'
  ) %>% mutate(
    w_unit = w_unit / mean(w_unit),
    Y_w = w_unit * Y
  ) %>% select(w_unit, Y_w) -> st_iid
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
  w_ps <- rep(1, n)
  for (i in 1:n) {
    w_ps[i] <- (N_cell/n_cell)[J_use == cell_id[i]]
  }
  w_ps <- w_ps/mean(w_ps)
  wght_sd_rt_ps[r, ] <- c(sd(w_ps), max(w_ps)/min(w_ps))


  mu_ps <- mean(w_ps * dat$Y)
  bias_mu_ps[r] <- mu_ps - mean(Y)
  sd_mu_ps[r] <- sqrt(sum(w_ps^2 * var(dat$Y)))/n
  cr_mu_ps[r] <- as.numeric(mu_ps - 1.96 * sd_mu_ps[r] <= mean(Y) & mean(Y) <= mu_ps + 1.96 * sd_mu_ps[r])
  ###------inverse-prob weighted estimator------###
  w_ips <- 1/sel_prob[I]/mean(1/sel_prob[I])

  wght_sd_rt_ips[r, ] <- c(sd(w_ips), max(w_ips)/min(w_ips))

  mu_ips <- mean(w_ips * dat$Y)
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


  wght_sd_rt_rake[r, ] <- c(sd(w_rake), max(w_rake)/min(w_rake))


  mu_rake <- mean(w_rake * dat$Y)
  bias_mu_rake[r] <- mu_rake - mean(Y)
  sd_mu_rake[r] <- sqrt(sum(w_rake^2 * var(dat$Y)))/n
  cr_mu_rake[r] <- as.numeric(mu_rake - 1.96 * sd_mu_rake[r] <= mean(Y) & mean(Y) <= mu_rake + 1.96 * sd_mu_rake[r])

  ### sub domain
  st_est_sm <- rep(0, dim(output$mu_cell_pred)[1])
  iid_est_sm <- rep(0, dim(output$mu_cell_pred)[1])
  
  w_unit <- (output$w_unit[order(agg_pop$pop_cell_id)])[cell_id]
  w_unit_iid <- (output_iid$w_unit[order(agg_pop$pop_cell_id)])[cell_id]

  for (v in 1:q) {
    for (l in 1:l_v[v + 1]) {
      cell_index <- (1:J_true)[cell_str[, v] == l]
      id_index <- cell_id %in% (1:J_true)[cell_str[, v] == l]
      id_cell <- agg_pop %>% mutate(cell_idx = row_number()) %>% filter(pop_cell_id %in% cell_index) %>% .$cell_idx
      mar_true <- sum(mu_cell_true[cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
      # model prediction
      st_est_sm <- output$mu_cell_pred[,id_cell] %*% (agg_pop$N[id_cell] / agg_pop$N[id_cell])
      # for (s in 1:dim(output$mu_cell_pred)[1]) {
      #   st_est_sm[s] <- sum(output$mu_cell_pred[s, cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
      # }
      # st_est_sm<-apply(output$mu_cell_pred[,cell_index] *
      # N_cell_true[cell_index],1,sum)/sum(N_cell_true[cell_index])
      bias_sub_st[r, l + sum(l_v[1:v])] <- mean(st_est_sm) - mar_true
      sd_sub_st[r, l + sum(l_v[1:v])] <- sd(st_est_sm)
      if (quantile(st_est_sm, 0.025) <= mar_true & mar_true <= quantile(st_est_sm, 0.975)) {
        cr_sub_st[r, l + sum(l_v[1:v])] <- 1
      }

      # independent prior
      iid_est_sm <- output_iid$mu_cell_pred[,id_cell] %*% (agg_pop$N[id_cell] / agg_pop$N[id_cell])
      # for (s in 1:dim(output$mu_cell_pred)[1]) {
      #   iid_est_sm[s] <- sum(output_iid$mu_cell_pred[s, cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
      # }
      bias_sub_iid[r, l + sum(l_v[1:v])] <- mean(iid_est_sm) - mar_true
      sd_sub_iid[r, l + sum(l_v[1:v])] <- sd(iid_est_sm)
      if (quantile(iid_est_sm, 0.025) <= mar_true & mar_true <= quantile(iid_est_sm, 0.975)) {
        cr_sub_iid[r, l + sum(l_v[1:v])] <- 1
      }
      # model-based weights under st prior
      est_st_wt <- sum(w_unit[id_index] * dat$Y[id_index])/sum(w_unit[id_index])
      bias_sub_st_wt[r, l + sum(l_v[1:v])] <- est_st_wt - mar_true
      sd_sub_st_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(w_unit[id_index]^2 * var(dat$Y[id_index])))/sum(w_unit[id_index])
      cr_sub_st_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_st_wt - 1.96 * sd_sub_st_wt[r, l + sum(l_v[1:v])] <=
                                                         mar_true & mar_true <= est_st_wt + 1.96 * sd_sub_st_wt[r, l + sum(l_v[1:v])])

      # model-based weights under independent prior
      est_iid_wt <- sum(w_unit_iid[id_index] * dat$Y[id_index])/sum(w_unit_iid[id_index])
      bias_sub_iid_wt[r, l + sum(l_v[1:v])] <- est_iid_wt - mar_true
      sd_sub_iid_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(w_unit_iid[id_index]^2 * var(dat$Y[id_index])))/sum(w_unit_iid[id_index])
      cr_sub_iid_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_iid_wt - 1.96 * sd_sub_iid_wt[r, l + sum(l_v[1:v])] <=
                                                          mar_true & mar_true <= est_iid_wt + 1.96 * sd_sub_iid_wt[r, l + sum(l_v[1:v])])

      # ps weights
      est_ps_wt <- sum(w_ps[id_index] * dat$Y[id_index])/sum(w_ps[id_index])
      bias_sub_ps_wt[r, l + sum(l_v[1:v])] <- est_ps_wt - mar_true
      sd_sub_ps_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(w_ps[id_index]^2 * var(dat$Y[id_index])))/sum(w_ps[id_index])
      cr_sub_ps_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_ps_wt - 1.96 * sd_sub_ps_wt[r, l + sum(l_v[1:v])] <=
                                                         mar_true & mar_true <= est_ps_wt + 1.96 * sd_sub_ps_wt[r, l + sum(l_v[1:v])])
      # ips weights
      est_ips_wt <- sum(w_ips[id_index] * dat$Y[id_index])/sum(w_ips[id_index])
      bias_sub_ips_wt[r, l + sum(l_v[1:v])] <- est_ips_wt - mar_true
      sd_sub_ips_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(w_ips[id_index]^2 * var(dat$Y[id_index])))/sum(w_ips[id_index])
      cr_sub_ips_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_ips_wt - 1.96 * sd_sub_ips_wt[r, l + sum(l_v[1:v])] <=
                                                          mar_true & mar_true <= est_ips_wt + 1.96 * sd_sub_ips_wt[r, l + sum(l_v[1:v])])

      # rake weights
      est_rake_wt <- sum(w_rake[id_index] * dat$Y[id_index])/sum(w_rake[id_index])
      bias_sub_rake_wt[r, l + sum(l_v[1:v])] <- est_rake_wt - mar_true
      sd_sub_rake_wt[r, l + sum(l_v[1:v])] <- sqrt(sum(w_rake[id_index]^2 * var(dat$Y[id_index])))/sum(w_rake[id_index])
      cr_sub_rake_wt[r, l + sum(l_v[1:v])] <- as.numeric(est_rake_wt - 1.96 * sd_sub_rake_wt[r, l + sum(l_v[1:v])] <=
                                                           mar_true & mar_true <= est_rake_wt + 1.96 * sd_sub_rake_wt[r, l + sum(l_v[1:v])])

    }
  }
  # interaction
  cell_index <- (1:J_true)[cell_str[, 1] == 1 & cell_str[, 2] > 1]
  id_index <- cell_id %in% (1:J_true)[cell_str[, 1] == 1 & cell_str[, 2] > 1]
  mar_true <- sum(mu_cell_true[cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
  id_cell <- agg_pop %>% mutate(cell_idx = row_number()) %>% filter(pop_cell_id %in% cell_index) %>% .$cell_idx

  # model prediction
  st_est_sm <- output$mu_cell_pred[,id_cell] %*% (agg_pop$N[id_cell] / agg_pop$N[id_cell])
  # for (s in 1:dim(output$mu_cell_pred)[1]) {
  #   st_est_sm[s] <- sum(output$mu_cell_pred[s, cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
  # }
  bias_sub_st_int[r] <- mean(st_est_sm) - mar_true
  sd_sub_st_int[r] <- sd(st_est_sm)
  if (quantile(st_est_sm, 0.025) <= mar_true & mar_true <= quantile(st_est_sm, 0.975)) {
    cr_sub_st_int[r] <- 1
  }

  # independent prior
  iid_est_sm <- output_iid$mu_cell_pred[,id_cell] %*% (agg_pop$N[id_cell] / agg_pop$N[id_cell])
  # for (s in 1:dim(output$mu_cell_pred)[1]) {
  #   iid_est_sm[s] <- sum(output_iid$mu_cell_pred[s, cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
  # }
  bias_sub_iid_int[r] <- mean(iid_est_sm) - mar_true
  sd_sub_iid_int[r] <- sd(iid_est_sm)
  if (quantile(iid_est_sm, 0.025) <= mar_true & mar_true <= quantile(iid_est_sm, 0.975)) {
    cr_sub_iid_int[r] <- 1
  }

  # model-based weights under st prior
  est_st_wt <- sum(w_unit[id_index] * dat$Y[id_index])/sum(w_unit[id_index])
  bias_sub_st_wt_int[r] <- est_st_wt - mar_true
  sd_sub_st_wt_int[r] <- sqrt(sum(w_unit[id_index]^2 * var(dat$Y[id_index])))/sum(w_unit[id_index])
  cr_sub_st_wt_int[r] <- as.numeric(est_st_wt - 1.96 * sd_sub_st_wt_int[r] <= mar_true & mar_true <= est_st_wt +
                                      1.96 * sd_sub_st_wt_int[r])

  # model-based weights under independent prior
  est_iid_wt <- sum(w_unit_iid[id_index] * dat$Y[id_index])/sum(w_unit_iid[id_index])
  bias_sub_iid_wt_int[r] <- est_iid_wt - mar_true
  sd_sub_iid_wt_int[r] <- sqrt(sum(w_unit_iid[id_index]^2 * var(dat$Y[id_index])))/sum(w_unit_iid[id_index])
  cr_sub_iid_wt_int[r] <- as.numeric(est_iid_wt - 1.96 * sd_sub_iid_wt_int[r] <= mar_true & mar_true <= est_iid_wt +
                                       1.96 * sd_sub_iid_wt_int[r])

  # ps weights
  est_ps_wt <- sum(w_ps[id_index] * dat$Y[id_index])/sum(w_ps[id_index])
  bias_sub_ps_wt_int[r] <- est_ps_wt - mar_true
  sd_sub_ps_wt_int[r] <- sqrt(sum(w_ps[id_index]^2 * var(dat$Y[id_index])))/sum(w_ps[id_index])
  cr_sub_ps_wt_int[r] <- as.numeric(est_ps_wt - 1.96 * sd_sub_ps_wt_int[r] <= mar_true & mar_true <= est_ps_wt +
                                      1.96 * sd_sub_ps_wt_int[r])
  # ips weights
  est_ips_wt <- sum(w_ips[id_index] * dat$Y[id_index])/sum(w_ips[id_index])
  bias_sub_ips_wt_int[r] <- est_ips_wt - mar_true
  sd_sub_ips_wt_int[r] <- sqrt(sum(w_ips[id_index]^2 * var(dat$Y[id_index])))/sum(w_ips[id_index])
  cr_sub_ips_wt_int[r] <- as.numeric(est_ips_wt - 1.96 * sd_sub_ips_wt_int[r] <= mar_true & mar_true <= est_ips_wt +
                                       1.96 * sd_sub_ips_wt_int[r])

  # rake weights
  est_rake_wt <- sum(w_rake[id_index] * dat$Y[id_index])/sum(w_rake[id_index])
  bias_sub_rake_wt_int[r] <- est_rake_wt - mar_true
  sd_sub_rake_wt_int[r] <- sqrt(sum(w_rake[id_index]^2 * var(dat$Y[id_index])))/sum(w_rake[id_index])
  cr_sub_rake_wt_int[r] <- as.numeric(est_rake_wt - 1.96 * sd_sub_rake_wt_int[r] <= mar_true & mar_true <= est_rake_wt +
                                        1.96 * sd_sub_rake_wt_int[r])
     
}  #end of repeated sampling
