###---------------MRP to Construct Survey Weights--------------###
### Author: YS & RT & JG Latest Edit date: 07/19/2017 clear
remove(list = objects())

#----------required packages-----------------#
library(rstanarm)
library(survey)
library(ggplot2)
library(dplyr)
library(directlabels)

if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("stan-dev/rstanarm", args = "--preclean", build_vignettes = FALSE, ref = 'structured_prior_merge')

set.seed(20150213)

source('helper_functions.R')

acs_ad <- readRDS('data/acs_ad.RDS')

q <- 8

J_age <- length(unique(acs_ad$age))
J_eth <- length(unique(acs_ad$eth))
J_edu <- length(unique(acs_ad$edu))
J_sex <- length(unique(acs_ad$sex))
J_inc <- length(unique(acs_ad$inc))
J_eld <- length(unique(acs_ad$eld))
J_cld <- length(unique(acs_ad$cld))
J_ps <- length(unique(acs_ad$ps))

N <- dim(acs_ad)[1]

options("contrasts")
age <- model.matrix(~age, acs_ad)
eth <- model.matrix(~eth, acs_ad)
edu <- model.matrix(~edu, acs_ad)
sex <- model.matrix(~sex, acs_ad)
inc <- model.matrix(~inc, acs_ad)
eld <- model.matrix(~eld, acs_ad)
cld <- model.matrix(~cld, acs_ad)
ps <- model.matrix(~ps, acs_ad)

age_eth <- model.matrix(~age:eth, acs_ad)[, -1]
age_edu <- model.matrix(~age:edu, acs_ad)[, -1]
eth_edu <- model.matrix(~eth:edu, acs_ad)[, -1]

eth_inc <- model.matrix(~eth:inc, acs_ad)[, -1]

age_inc <- model.matrix(~age:inc, acs_ad)[, -1]

inc_ps <- model.matrix(~inc:ps, acs_ad)[, -1]
inc_cld <- model.matrix(~inc:cld, acs_ad)[, -1]
inc_eld <- model.matrix(~inc:eld, acs_ad)[, -1]

age_eth_edu <- model.matrix(~age:eth:edu, acs_ad)[, -1]
age_eth_inc <- model.matrix(~age:eth:inc, acs_ad)[, -1]

### simulate Y

# case 2)
betaY_age <- matrix(sample(-2:2, dim(age)[2], replace = T), dim(age)[2], 1)
betaY_eth <- matrix(sample(-2:2, dim(eth)[2], replace = T), dim(eth)[2], 1)
betaY_edu <- matrix(sample(-2:2, dim(edu)[2], replace = T), dim(edu)[2], 1)
betaY_sex <- matrix(sample(-2:2, dim(sex)[2], replace = T), dim(sex)[2], 1)
betaY_inc <- matrix(sample(-2:2, dim(inc)[2], replace = T), dim(inc)[2], 1)
betaY_cld <- matrix(0, dim(cld)[2], 1)
betaY_eld <- matrix(0, dim(eld)[2], 1)
betaY_ps <- matrix(0, dim(ps)[2], 1)

betaY_age_eth <- matrix(0, dim(age_eth)[2], 1)
betaY_age_edu <- matrix(0, dim(age_edu)[2], 1)
betaY_eth_edu <- matrix(0, dim(eth_edu)[2], 1)

betaY_eth_inc <- matrix(0, dim(eth_inc)[2], 1)
betaY_age_inc <- matrix(0, dim(age_inc)[2], 1)
betaY_inc_ps <- matrix(0, dim(inc_ps)[2], 1)
betaY_inc_eld <- matrix(0, dim(inc_eld)[2], 1)
betaY_inc_cld <- matrix(0, dim(inc_cld)[2], 1)

betaY_age_eth_edu <- matrix(0, dim(age_eth_edu)[2], 1)
betaY_age_eth_inc <- matrix(0, dim(age_eth_inc)[2], 1)

muY <- 
  age %*% betaY_age +
  eth %*% betaY_eth +
  edu %*% betaY_edu +
  sex %*% betaY_sex + 
  inc %*% betaY_inc + 
  ps %*% betaY_ps +
  eld %*% betaY_eld +
  cld %*% betaY_cld +
  age_eth %*% betaY_age_eth +
  age_edu %*% betaY_age_edu +
  eth_edu %*% betaY_eth_edu +
  eth_inc %*% betaY_eth_inc +
  age_inc %*% betaY_age_inc +
  inc_ps %*% betaY_inc_ps + 
  inc_eld %*% betaY_inc_eld +
  inc_cld %*% betaY_inc_cld +
  age_eth_edu %*% betaY_age_eth_edu +
  age_eth_inc %*% betaY_age_eth_inc

Y <- rnorm(N) * 1 + muY

### simuate I

# #case 2)
betaI_age <- matrix(seq(0, 3, length = dim(age)[2]), dim(age)[2], 1)
betaI_eth <- matrix(seq(-1, 1, length = dim(eth)[2]), dim(eth)[2], 1)
betaI_edu <- matrix(seq(0, 2, length = dim(edu)[2]), dim(edu)[2], 1)
betaI_sex <- matrix(seq(-1, 0, length = dim(sex)[2]), dim(sex)[2], 1)
betaI_inc <- matrix(seq(0, 4, length = dim(inc)[2]), dim(inc)[2], 1)
betaI_cld <- matrix(seq(-1, 1, length = dim(cld)[2]), dim(cld)[2], 1)
betaI_eld <- matrix(seq(-2, 0, length = dim(eld)[2]), dim(eld)[2], 1)
betaI_ps <- matrix(seq(-1, 0, length = dim(ps)[2]), dim(ps)[2], 1)

betaI_age_eth <- matrix(0, dim(age_eth)[2], 1)
betaI_age_edu <- matrix(0, dim(age_edu)[2], 1)
betaI_eth_edu <- matrix(0, dim(eth_edu)[2], 1)

betaI_eth_inc <- matrix(0, dim(eth_inc)[2], 1)
betaI_age_inc <- matrix(0, dim(age_inc)[2], 1)
betaI_inc_ps <- matrix(0, dim(inc_ps)[2], 1)
betaI_inc_eld <- matrix(0, dim(inc_eld)[2], 1)
betaI_inc_cld <- matrix(0, dim(inc_cld)[2], 1)

betaI_age_eth_edu <- matrix(0, dim(age_eth_edu)[2], 1)
betaI_age_eth_inc <- matrix(0, dim(age_eth_inc)[2], 1)

sel_prob <- 
  plogis(
    -2 +
      age %*% betaI_age +
      eth %*% betaI_eth +
      edu %*% betaI_edu +
      sex %*% betaI_sex +
      inc %*% betaI_inc +
      ps %*% betaI_ps +
      eld %*% betaI_eld +
      cld %*% betaI_cld +
      age_eth %*% betaI_age_eth + 
      age_edu %*% betaI_age_edu +
      eth_edu %*% betaI_eth_edu +
      eth_inc %*% betaI_eth_inc +
      age_inc %*% betaI_age_inc +
      inc_ps %*% betaI_inc_ps +
      inc_eld %*% betaI_inc_eld +
      inc_cld %*% betaI_inc_cld +
      age_eth_edu %*% betaI_age_eth_edu +
      age_eth_inc %*% betaI_age_eth_inc
    )

J_age <- length(unique(acs_ad$age))
J_eth <- length(unique(acs_ad$eth))
J_edu <- length(unique(acs_ad$edu))
J_sex <- length(unique(acs_ad$sex))
J_inc <- length(unique(acs_ad$inc))
J_eld <- length(unique(acs_ad$eld))
J_cld <- length(unique(acs_ad$cld))
J_ps <- length(unique(acs_ad$ps))

acs_ad %>% 
  mutate(
    pop_cell_id =  paste0(age, eth, edu, sex, inc, eld, cld, ps),
    j = (ps_int - 1) * J_cld * J_eld * J_inc * J_sex * J_edu * J_eth * J_age +
        (cld_int - 1) * J_eld * J_inc * J_sex * J_edu * J_eth * J_age +
        (eld_int - 1) * J_inc * J_sex * J_edu * J_eth * J_age + 
        (inc_int - 1) * J_sex * J_edu * J_eth * J_age +
        (sex_int - 1) * J_edu * J_eth * J_age +
        (edu_int - 1) * J_eth * J_age +
        (eth_int - 1) * J_age +
        age_int
  ) -> acs_ad

pop_cell_id <- acs_ad$pop_cell_id

agg_pop <- acs_ad %>%
  mutate(
    Y = as.vector(Y[,1])
  ) %>%
  group_by(
    age, eth, edu,
    sex, inc, eld,
    cld, ps
  ) %>%
  summarise(
    N = n(),
    Y = mean(Y),
    cell_id = first(pop_cell_id),
    j = first(j)
  ) %>%
  ungroup()

###------compiling------###
I <- (runif(N) <= sel_prob)

dat <- 
  acs_ad[I,, drop=FALSE] %>% 
  select(
    age, eth, edu, sex,
    inc, eld, cld, ps, j 
  ) %>%
  mutate(
    Y = Y[I], 
    cell_id = pop_cell_id[I]
  )

dat_rstanarm <- dat %>% 
  group_by(
    age, eth, edu, sex, 
    inc, eld, cld, ps
  ) %>%
  summarise(
    sd_cell = sd(Y),
    n = n(),
    Y = mean(Y),
    cell_id = first(cell_id),
    j = first(j)
  ) %>%
  mutate(
    sd_cell = if_else(is.na(sd_cell), 0, sd_cell)
  ) %>%
  left_join(
    agg_pop[,c('cell_id','N')], by = 'cell_id'
  )
  
#-----------computation--------------#

###-----------------STAN with structural prior--------------------------###

ff <- as.formula(Y ~ 1 + (1 | age) + (1 | eth) + (1 | edu) + (1 | sex) +
                   (1 | inc) + (1 | eld) + (1 | cld) + (1 | ps) +
                   (1 | age:eth) + (1 | age:edu) + (1 | eth:edu) + (1 | eth:inc) +
                   (1 | age:inc) + (1 | inc:ps) + (1 | inc:eld) + (1 | inc:cld) +
                   (1 | age:eth:edu) + (1 | age:eth:inc))

S_st <-
stan_glmer(
  formula = ff,
  data = dat_rstanarm,
  iter = 500,
  chains = 4,
  cores = 1,
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



colnames(as.matrix(S_st))

draws_st <- as.matrix(S_st, pars = colnames(as.matrix(S_st))[403:432])

for (j in 404:413){
  j <- 416
j <- j-402 #413
print(summary(draws_st[,j]))
}
plot(draws_st[,j])

quantile(sqrt(pri_var(S_st)$sigma_y_sq),prob=c(0.025,0.5,0.975))
plot(pri_var(S_st)$sigma_y_sq)
summary(sqrt(pri_var(S_st)$sigma_theta_sq))
quantile(sqrt(pri_var(S_st)$sigma_theta_sq),prob=c(0.025,0.5,0.975))

summary(apply(shrinkage_factor(S_st,dat_rstanarm$n),2,median))

output_st <- sum_svey_model(S_st, agg_pop)

###-----------------STAN with independent prior--------------------------###

S_iid <-
stan_glmer(
  formula = ff,
  data = dat_rstanarm,
  iter = 500,
  chains = 4,
  cores = 1,
  prior_covariance = 
    mrp_structured(
      indep = TRUE,
      cell_size = dat_rstanarm$n, 
      cell_sd = dat_rstanarm$sd_cell
    ),
  seed = 123,
  prior_aux = cauchy(0, 5),
  prior_intercept = normal(0, 100, autoscale = FALSE),
  adapt_delta = 0.99
)

colnames(as.matrix(S_iid))

draws_iid <- as.matrix(S_iid, pars = colnames(as.matrix(S_iid))[403:421])

for (j in 404:421){
  j=410
  j <- j-402 #413
  print(summary(draws_iid[,j]))
}

plot(draws_iid[,j])

output_iid <- sum_svey_model(S_iid, agg_pop)

### model-based weights
st_out <-
  dat %>%
  left_join(
    output_st$mean_w_new, by = 'cell_id'
  ) %>%
  mutate(
    w = w_unit / mean(w_unit),
    Y_w = w * Y
  ) %>%
  select(cell_id, w, Y_w, Y)

output_st$mu_w <- mean(st_out$Y_w)
output_st$w_unit <- st_out$w

st.dat.design <- svydesign(id = ~1, data = dat, weights = output_st$w)
# svymean(~Y, st.dat.design)

iid_out <-
  dat %>%
  left_join(
    output_iid$mean_w_new, by = 'cell_id'
  ) %>%
  mutate(
    w = w_unit / mean(w_unit),
    Y_w = w * Y
  ) %>%
  select(cell_id, w, Y_w, Y)

output_iid$mu_w <- mean(iid_out$Y_w)
output_iid$w_unit <- iid_out$w

id.dat.design <- svydesign(id = ~1, data = dat, weights = output_iid$w_unit)

w_ps_df <-
  dat[,c('cell_id','Y')] %>%
  left_join(
    agg_pop[,c('cell_id','N')], by = 'cell_id'
  ) %>%
  left_join(
    dat_rstanarm[,c('cell_id','n')], by = 'cell_id'
  ) %>%
  mutate(
    w = N / n,
    w = w / mean(w),
    Y_w = w * Y
  )

###------PS weighting------###
ps.dat.design <- svydesign(id = ~1, data = dat, weights = w_ps_df$w)
#
# # svymean(~Y, ps.dat.design)
#
###------inverse-prob weighted estimator------###

w_ips <- 1 / sel_prob[I] / mean(1/sel_prob[I])
w_ips_df <-
  data.frame(
    w = w_ips,
    Y = dat$Y,
    cell_id = dat$cell_id
  ) %>%
  mutate(
    Y_w = w * Y
  )

ips.dat.design <- svydesign(id = ~1, data = dat, weights = w_ips_df$w)

# svymean(~Y, ips.dat.design)

###------raking estimator------###
dat.design <- svydesign(id = ~1, data = dat)

pop.age <- data.frame(1:J_age, Freq = as.numeric(table(acs_ad$age)))
pop.eth <- data.frame(1:J_eth, Freq = as.numeric(table(acs_ad$eth)))
pop.edu <- data.frame(1:J_edu, Freq = as.numeric(table(acs_ad$edu)))
pop.sex <- data.frame(1:J_sex, Freq = as.numeric(table(acs_ad$sex)))
pop.inc <- data.frame(1:J_inc, Freq = as.numeric(table(acs_ad$inc)))
pop.eld <- data.frame(1:J_eld, Freq = as.numeric(table(acs_ad$eld)))
pop.cld <- data.frame(1:J_cld, Freq = as.numeric(table(acs_ad$cld)))
pop.ps <- data.frame(1:J_ps, Freq = as.numeric(table(acs_ad$ps)))

names(pop.age) <- c("age", "Freq")
names(pop.eth) <- c("eth", "Freq")
names(pop.edu) <- c("edu", "Freq")
names(pop.sex) <- c("sex", "Freq")
names(pop.inc) <- c("inc", "Freq")
names(pop.eld) <- c("eld", "Freq")
names(pop.cld) <- c("cld", "Freq")
names(pop.ps) <- c("ps", "Freq")

dat_rake <-
  rake(
    dat.design,
    list(~age, ~eth, ~edu, ~sex, ~inc, ~cld, ~eld, ~ps),
    list(pop.age, pop.eth, pop.edu, pop.sex, pop.inc, pop.cld, pop.eld, pop.ps),
    control = list(maxit = 20, epsilon = 1, verbose = FALSE)
  )

w_rake <- weights(dat_rake)/mean(weights(dat_rake))
w_rake_df <-
  data.frame(
    w = w_rake,
    Y = dat$Y,
    cell_id = dat$cell_id
  ) %>%
  mutate(
    Y_w = w * Y
  )

rake.dat.design <- svydesign(id = ~1, data = dat, weights = w_rake_df$w)
# svymean(~Y, rake.dat.design)

### marginal means sub domain
l_v <- c(0, J_age, J_eth, J_edu, J_sex, J_inc, J_eld, J_cld, J_ps)

bias_sub_st <-
  sd_sub_st <-
  bias_sub_st_wt <-
  sd_sub_st_wt <-
  bias_sub_st_wt2 <-
  sd_sub_st_wt2 <-
  bias_sub_iid <-
  sd_sub_iid <-
  bias_sub_iid_wt <-
  sd_sub_iid_wt <-
  bias_sub_iid_wt2 <-
  sd_sub_iid_wt2 <-
  bias_sub_ps_wt <-
  sd_sub_ps_wt <-
  bias_sub_ips_wt <-
  sd_sub_ips_wt <-
  bias_sub_rake_wt <-
  sd_sub_rake_wt <- rep(0, sum(l_v))

cell_str <- agg_pop[,c('age','eth','edu','sex','inc','eld','cld','ps')]
for (v in 1:q) {
  for (l in 1:l_v[v + 1]) {
    sub_pop_data <- agg_pop[which(cell_str[,v] == l),c('cell_id','Y','N')]
    sub_cell_idx <- sub_pop_data$cell_id
    mar_true <- sum(sub_pop_data$Y * sub_pop_data$N/sum(sub_pop_data$N))

    st_est_sm <- output_st$mu_cell_pred[,sub_cell_idx] %*% (sub_pop_data$N / sum(sub_pop_data$N))
    bias_sub_st[l + sum(l_v[1:v])] <- mean(st_est_sm) - mar_true
    sd_sub_st[l + sum(l_v[1:v])] <- sd(st_est_sm)

    # independent prior
    iid_est_sm <- output_iid$mu_cell_pred[,sub_cell_idx] %*%
                                        (sub_pop_data$N / sum(sub_pop_data$N))
    bias_sub_iid[l + sum(l_v[1:v])] <- mean(iid_est_sm) - mar_true
    sd_sub_iid[l + sum(l_v[1:v])] <- sd(iid_est_sm)

    # model-based weights under st prior
    w_sum <- sum_weights(weight_df = st_out, idx = sub_cell_idx, comp_stat = mar_true)
    bias_sub_st_wt[l + sum(l_v[1:v])] <- w_sum$bias
    sd_sub_st_wt[l + sum(l_v[1:v])] <- w_sum$sd_wt

    # model-based weights under independent prior
    w_sum <- sum_weights(weight_df = iid_out, idx = sub_cell_idx, comp_stat = mar_true)
    bias_sub_iid_wt[l + sum(l_v[1:v])] <- w_sum$bias
    sd_sub_iid_wt[l + sum(l_v[1:v])] <- w_sum$sd_wt

    # ps weights
    w_sum <- sum_weights(weight_df = w_ps_df, idx = sub_cell_idx, comp_stat = mar_true)
    bias_sub_ps_wt[l + sum(l_v[1:v])] <- w_sum$bias
    sd_sub_ps_wt[l + sum(l_v[1:v])] <- w_sum$sd_wt

    # ips weights
    w_sum <- sum_weights(weight_df = w_ips_df, idx = sub_cell_idx, comp_stat = mar_true)
    bias_sub_ips_wt[l + sum(l_v[1:v])] <- w_sum$bias
    sd_sub_ips_wt[l + sum(l_v[1:v])] <- w_sum$sd_wt

    # rake weights
    w_sum <- sum_weights(weight_df = w_rake_df, idx = sub_cell_idx, comp_stat = mar_true)
    bias_sub_rake_wt[l + sum(l_v[1:v])] <- w_sum$bias
    sd_sub_rake_wt[l + sum(l_v[1:v])] <- w_sum$sd_wt
  }
}

###-----------interaction---------------###
sub_pop_data <- agg_pop[which(cell_str$age == 1 &
                                cell_str$eth == 2 &
                                as.integer(cell_str$edu) > 2),c('cell_id','Y','N')]
sub_cell_idx <- sub_pop_data$cell_id
mar_true <- sum(sub_pop_data$Y * sub_pop_data$N/sum(sub_pop_data$N))

# model prediction
st_est_sm <- output_st$mu_cell_pred[,sub_cell_idx] %*% (sub_pop_data$N / sum(sub_pop_data$N))
bias_sub_st_int <- mean(st_est_sm) - mar_true
sd_sub_st_int <- sd(st_est_sm)

# independent prior
iid_est_sm <- output_iid$mu_cell_pred[,sub_cell_idx] %*%
                                    (sub_pop_data$N / sum(sub_pop_data$N))
bias_sub_iid_int <- mean(iid_est_sm) - mar_true
sd_sub_iid_int <- sd(iid_est_sm)

# model-based weights under st prior
w_sum <- sum_weights(weight_df = st_out, idx = sub_cell_idx, comp_stat = mar_true)
bias_sub_st_wt_int <- w_sum$bias
sd_sub_st_wt_int <- w_sum$sd_wt

# model-based weights under independent prior
w_sum <- sum_weights(weight_df = iid_out, idx = sub_cell_idx, comp_stat = mar_true)
bias_sub_iid_wt_int <- w_sum$bias
sd_sub_iid_wt_int <- w_sum$sd_wt

# ps weights
w_sum <- sum_weights(weight_df = w_ps_df, idx = sub_cell_idx, comp_stat = mar_true)
bias_sub_ps_wt_int <- w_sum$bias
sd_sub_ps_wt_int <- w_sum$sd_wt

# ips weights
w_sum <- sum_weights(weight_df = w_ips_df, idx = sub_cell_idx, comp_stat = mar_true)
bias_sub_ips_wt_int <- w_sum$bias
sd_sub_ips_wt_int <- w_sum$sd_wt

# rake weights
w_sum <- sum_weights(weight_df = w_rake_df, idx = sub_cell_idx, comp_stat = mar_true)
bias_sub_rake_wt_int <- w_sum$bias
sd_sub_rake_wt_int <- w_sum$sd_wt

output <- output_st
w_unit <- st_out$w
w_unit_iid <- iid_out$w
w_ps <- w_ps_df$w
w_ips <- w_ips_df$w
w_rake <- w_rake_df$w

n <- nrow(dat)
over_mean_wgt <- data.frame(cbind(c(sd(w_unit), max(w_unit)/min(w_unit), mean(output$theta_pred) - mean(Y), sd(output$theta_pred)),
    c(sd(w_unit_iid), max(w_unit_iid)/min(w_unit_iid), mean(output_iid$theta_pred) - mean(Y), sd(output_iid$theta_pred)),
    c(sd(w_unit), max(w_unit)/min(w_unit), sum(w_unit * dat$Y)/sum(w_unit) - mean(Y), sqrt(sum(w_unit^2 * var(dat$Y)))/n),
    c(sd(w_unit_iid), max(w_unit_iid)/min(w_unit_iid), sum(w_unit_iid * dat$Y)/sum(w_unit_iid) - mean(Y), sqrt(sum(w_unit_iid^2 *
        var(dat$Y)))/n), c(sd(w_ps), max(w_ps)/min(w_ps), sum(w_ps * dat$Y)/sum(w_ps) - mean(Y), sqrt(sum(w_ps^2 *
        var(dat$Y)))/n), c(sd(w_rake), max(w_rake)/min(w_rake), sum(w_rake * dat$Y)/sum(w_rake) - mean(Y), sqrt(sum(w_rake^2 *
        var(dat$Y)))/n), c(sd(w_ips), max(w_ips)/min(w_ips), sum(w_ips * dat$Y)/sum(w_ips) - mean(Y), sqrt(sum(w_ips^2 *
        var(dat$Y)))/n)))

mar_wgt_mean <- data.frame(cbind(c(bias_sub_st,sd_sub_st),c(bias_sub_iid,sd_sub_iid),
                             c(bias_sub_st_wt,sd_sub_st_wt),c(bias_sub_iid_wt,sd_sub_iid_wt),
                            c(bias_sub_ps_wt,sd_sub_ps_wt),c(bias_sub_rake_wt,sd_sub_rake_wt),
                            c(bias_sub_ips_wt,sd_sub_ips_wt)))

int_wgt_mean <- data.frame(cbind(c(bias_sub_st_int,sd_sub_st_int,bias_sub_iid_int,sd_sub_iid_int)),
    c(mean(bias_sub_st_wt_int,sd_sub_st_wt_int,bias_sub_iid_wt_int,sd_sub_iid_wt_int)),
    c(mean(bias_sub_ps_wt_int,sd_sub_ps_wt_int,bias_sub_rake_wt_int,sd_sub_rake_wt_int)),
    c(mean(bias_sub_ips_wt_int,sd_sub_ips_wt_int)))


weights <- cbind(c(w_unit, w_rake, w_ips, w_ps, w_unit_iid), c(rep("Str-W", n), rep("Rake-W", n), rep("IP-W",
    n), rep("PS-W", n), rep("Ind-W", n)))
weights <- data.frame(weights)
names(weights) <- c("wt", "Method")
weights$wt <- log(as.numeric(as.character(weights$wt)))
weights$Method <- factor(weights$Method, levels = c("Str-W", "Rake-W", "IP-W", "PS-W", "Ind-W"))
#
direct.label(ggplot(weights[1:(3 * n), ], aes(x = wt, group = Method)) + geom_density(aes(color = Method)) + theme_bw() +
    scale_x_continuous(name = "Distributions of log(weights)") + scale_y_continuous(name = "", expand = c(0, 0),
    limits = c(0, 0.8)) + theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_blank(), legend.position = "",
    legend.title = element_blank(), axis.text.x = element_text(size = 20), axis.title = element_text(size = 28),
    axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()))
ggsave("plot/weight-var8.pdf")


weights_s <- cbind(c(w_unit/sum(w_unit), w_rake/sum(w_rake), w_ips/sum(w_ips), w_ps/sum(w_ps), w_unit_iid/sum(w_unit_iid),
    rep(1/n, n)), c(rep("Str-W", n), rep("Rake-W", n), rep("IP-W", n), rep("PS-W", n), rep("Ind-W", n), rep("Sample",
    n)))

weights_y <- data.frame(cbind(weights_s, dat$Y))
weights_y <- rbind(weights_y, data.frame(cbind(rep(1/length(Y), length(Y)), rep("POP", length(Y)), Y)))

names(weights_y) <- c("w", "Method", "Y")
weights_y$w <- as.numeric(as.character(weights_y$w))
weights_y$Y <- as.numeric(as.character(weights_y$Y))
weights_y$Method <- factor(weights_y$Method, levels = c("Str-W", "Rake-W", "IP-W", "Ind-W", "PS-W", "POP", "Sample"))

direct.label(ggplot(weights_y[c(1:(3 * n), (5 * n + 1):dim(weights_y)[1]), ], aes(x = Y, weights = w, group = Method,
    color = Method)) + geom_density() + theme_bw() + scale_x_continuous(name = "Weighted distribution of outcome") +
    scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, 0.2)) + theme(axis.line.x = element_line(colour = "black"),
    axis.line.y = element_blank(), legend.position = "", legend.title = element_blank(), axis.text.x = element_text(size = 20),
    axis.title = element_text(size = 28), axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()))

ggsave("plot/weighted-density.pdf")

#se


se_out <- data.frame(cbind(c("age:18-34", "age:35-44", "age:45-54", "age:55-64", "age:65+", "white&non-Hisp", "black&non-Hisp",
    "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", ">=col", "male", "female", "pov-gap <50%",
    "pov-gap50-100%", "pov-gap100-200%", "pov-gap200-300%", "pov-gap300%+", "#eld=0", "#eld=1", "#eld>=2", "#cld=0",
    "#cld=1", "#cld=2", "#cld>=3", "#fam=1", "#fam=2", "#fam=3", "#fam>=4"), mar_wgt_mean[sum(l_v) + 1:sum(l_v), ]))

colnames(se_out) <- c("Quantity", "Str-P", "Ind-P", "Str-W", "Ind-W", "PS-W", "Rake-W", "IP-W")
se_out$Quantity <- factor(se_out$Quantity, levels = c("age:18-34", "age:35-44", "age:45-54", "age:55-64", "age:65+",
    "white&non-Hisp", "black&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", ">=col",
    "male", "female", "pov-gap <50%", "pov-gap50-100%", "pov-gap100-200%", "pov-gap200-300%", "pov-gap300%+",
    "#eld=0", "#eld=1", "#eld>=2", "#cld=0", "#cld=1", "#cld=2", "#cld>=3", "#fam=1", "#fam=2", "#fam=3", "#fam>=4"))  #, 'non-white young','overall'))

re_se_out <- data.frame(se_out[, 1], se_out[, 5]/se_out[, 4], se_out[, 6]/se_out[, 4])

colnames(re_se_out) <- c("Quantity", "Ind-W/Str-W", "PS-W/Str-W")
re_se_out$Quantity <- factor(re_se_out$Quantity, levels = c("age:18-34", "age:35-44", "age:45-54", "age:55-64",
    "age:65+", "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col",
    ">=col", "male", "female", "pov-gap <50%", "pov-gap50-100%", "pov-gap100-200%", "pov-gap200-300%", "pov-gap300%+",
    "#eld=0", "#eld=1", "#eld>=2", "#cld=0", "#cld=1", "#cld=2", "#cld>=3", "#fam=1", "#fam=2", "#fam=3", "#fam>=4"))  #, 'non-white young','overall'))

re_se_out.m <- reshape2::melt(re_se_out)

ggplot(re_se_out.m, aes(x = value, fill = variable)) + geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") +
    scale_x_continuous("Relative SE", breaks = seq(1, 1.1, by = 0.01), expand = c(0, 0)) +
    scale_y_continuous(name = "Count of margins", expand = c(0, 0)) + theme_bw() + theme(axis.line = element_line(colour = "black"),
    legend.position = c(0.8,0.95), legend.title = element_blank(), legend.text = element_text(size = 20), axis.text = element_text(size = 20),
    axis.title = element_text(size = 28), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank())

ggsave("plot/var8_w_se.pdf")


ratios <- cbind(apply(output_iid$mu_cell_pred, 2, sd)/apply(output$mu_cell_pred, 2, sd), rep("Relative SE", length(apply(output$mu_cell_pred, 2, sd))))
ratios <- data.frame(ratios)
names(ratios) <- c("ratios", "term")
ratios$ratios <- as.numeric(as.character(ratios$ratios))


ggplot(ratios, aes(x = ratios, fill = term)) + geom_histogram(binwidth = 0.05, alpha = 0.5, position = "identity") +
    scale_x_continuous("Relative SE", breaks = seq(0.9, 2, by = 0.1), expand = c(0, 0)) + scale_y_continuous("Count of cells",
    expand = c(0, 0)) + scale_fill_discrete(name = "", labels = c("Ind-P/Str-P")) + theme_bw() + theme(axis.line = element_line(colour = "black"),
    legend.position = c(0.8,0.95), legend.title = element_blank(), legend.text = element_text(size = 20), axis.text = element_text(size = 20),
    axis.title = element_text(size = 28), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank())

ggsave("plot/var8_p_se.pdf")

