###---------------MRP to Construct Survey Weights--------------###
### Author: YS Latest Edit date: 06/07/2017 clear
remove(list = objects())

#----------required packages-----------------#
require(foreign)
require(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
require(survey)
require(foreach)
require(doMC)
require(ggplot2)

set.seed(20150213)

#----------data simulation-----------------#
### ACS data
acs_pop <- read.dta("data/acs_nyc_2011_wpov1.dta", convert.factors = FALSE)
acs_ad <- acs_pop[as.numeric(acs_pop$age) >= 18, ]  #adults
## age
age_tr <- acs_ad$age  #at least 18
acs_ad$age_dc <- age_tr  #discretization
acs_ad$age_dc[age_tr <= 34] <- 1  #18-34
acs_ad$age_dc[age_tr <= 44 & age_tr > 34] <- 2
acs_ad$age_dc[age_tr <= 54 & age_tr > 44] <- 3
acs_ad$age_dc[age_tr <= 64 & age_tr > 54] <- 4
acs_ad$age_dc[age_tr > 64] <- 5

### sex
acs_ad$sex <- as.numeric(acs_ad$sex)

### race race 1 '1 White Non-Hispanic' 2 '2 Black Non-Hispanic' 3 '3 Asian' 4 '4 Other' 5 '5 Hispanic'
acs_ad$race_dc <- acs_ad$racex

# educat 4 recode qi5 (1/2=1) (3/4=2) (5/6=3) (7/8=4) (98/99=.), gen(educat)

# poverty gap: 1 Under 50% 2 50-100% 3 100-200% 4 200-300% 5 300%+
acs_ad$opmres_x <- 1
acs_ad$opmres_x[acs_ad$poverty <= 100 & acs_ad$poverty > 50] <- 2
acs_ad$opmres_x[acs_ad$poverty <= 200 & acs_ad$poverty > 100] <- 3
acs_ad$opmres_x[acs_ad$poverty <= 300 & acs_ad$poverty > 200] <- 4
acs_ad$opmres_x[acs_ad$poverty > 300] <- 5

# eldx
acs_ad$eldx_ca <- acs_ad$eldx  # 0 1 2+
acs_ad$eldx_ca[acs_ad$eldx > 1] <- 2
acs_ad$eldx_ca <- acs_ad$eldx_ca + 1  #change to positive integer

# childx 0 1 2 3
acs_ad$childx_ca <- acs_ad$childx
acs_ad$childx_ca[acs_ad$childx_ca > 2] <- 3
acs_ad$childx_ca <- acs_ad$childx_ca + 1

# wax 0 1 2 acs_ad$wax_ca<-acs_ad$wax acs_ad$wax_ca[acs_ad$wax>1]<-2

# personx 1-4
acs_ad$personx_ca <- acs_ad$personx
acs_ad$personx_ca[acs_ad$personx > 4] <- 4

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

# #case 2) betaY_age<-matrix(seq(0.5, 4,length=dim(age)[2]),dim(age)[2],1) betaY_race<-matrix(seq(-2,
# 2,length=dim(race)[2]),dim(race)[2],1) betaY_edu<-matrix(seq(3, 0,length=dim(edu)[2]),dim(edu)[2],1)
# betaY_age_race<-matrix(0,dim(age_race)[2],1) betaY_age_edu<-matrix(0,dim(age_edu)[2],1)
# betaY_race_edu<-matrix(0,dim(race_edu)[2],1) betaY_age_race_edu<-matrix(0,dim(age_race_edu)[2],1) #case 3)
# RACE betaY_age<-matrix(seq(0.5, 4,length=dim(age)[2]),dim(age)[2],1) betaY_race<-matrix(0,dim(race)[2],1)
# betaY_edu<-matrix(seq(3, 0,length=dim(edu)[2]),dim(edu)[2],1) betaY_age_race<-matrix(0,dim(age_race)[2],1)
# betaY_age_edu<-matrix(sample(-2:2,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)
# betaY_race_edu<-matrix(0,dim(race_edu)[2],1) betaY_age_race_edu<-matrix(0,dim(age_race_edu)[2],1)

muY <- age %*% betaY_age + race %*% betaY_race + edu %*% betaY_edu + age_race %*% betaY_age_race + age_edu %*% 
    betaY_age_edu + race_edu %*% betaY_race_edu + age_race_edu %*% betaY_age_race_edu

Y <- rnorm(N) + muY

### simuate I

# case 0) betaI_age<-matrix(seq(-2, -1,length=dim(age)[2]),dim(age)[2],1) betaI_race<-matrix(seq(-1,
# 2,length=dim(race)[2]),dim(race)[2],1) betaI_edu<-matrix(seq(0, 2,length=dim(edu)[2]),dim(edu)[2],1)
# betaI_age_race<-matrix(sample(-1:1,dim(age_race)[2],replace=T),dim(age_race)[2],1)
# betaI_age_edu<-matrix(sample(-1:1,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)
# betaI_race_edu<-matrix(sample(-1:1,dim(race_edu)[2],replace=T),dim(race_edu)[2],1)
# betaI_age_race_edu<-matrix(sample(seq(-1,1,by=0.2),dim(age_race_edu)[2],replace=T),dim(age_race_edu)[2],1)

# case 1)
betaI_age <- matrix(seq(-2, 0, length = dim(age)[2]), dim(age)[2], 1)
betaI_race <- matrix(seq(-1, 1, length = dim(race)[2]), dim(race)[2], 1)
betaI_edu <- matrix(seq(0, 3, length = dim(edu)[2]), dim(edu)[2], 1)

betaI_age_race <- matrix(0, dim(age_race)[2], 1)
betaI_age_edu <- matrix(0, dim(age_edu)[2], 1)
betaI_race_edu <- matrix(0, dim(race_edu)[2], 1)
betaI_age_race_edu <- matrix(0, dim(age_race_edu)[2], 1)

# case 2) betaI_age<-matrix(seq(0, 2,length=dim(age)[2]),dim(age)[2],1) betaI_race<-matrix(seq(-2,
# 0,length=dim(race)[2]),dim(race)[2],1) betaI_edu<-matrix(0,dim(edu)[2],1)
# betaI_age_race<-matrix(sample(-1:1,dim(age_race)[2],replace=T),dim(age_race)[2],1)
# betaI_age_edu<-matrix(0,dim(age_edu)[2],1) betaI_race_edu<-matrix(0,dim(race_edu)[2],1)
# betaI_age_race_edu<-matrix(0,dim(age_race_edu)[2],1)

sel_prob <- 1/(1 + exp(-(-2 + age %*% betaI_age + race %*% betaI_race + edu %*% betaI_edu + age_race %*% betaI_age_race + 
    age_edu %*% betaI_age_edu + race_edu %*% betaI_race_edu + age_race_edu %*% betaI_age_race_edu)))

# hist(sel_prob) sum((runif(N)<=sel_prob))

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

y_cell <- aggregate(. ~ cell_id, data = dat, mean)$Y
ss_cell <- 0
for (j in 1:J) {
    ss_cell <- ss_cell + sum((dat$Y[cell_id == J_use[j]] - y_cell[j])^2)
}

###-----------------STAN with structural prior--------------------------###

stan.data_cell <- list(n = nrow(dat), q = q, J = J, n_cell = n_cell, y_cell = y_cell, ss_cell = ss_cell, cell_str = cell_str, 
    N_cell = N_cell, J_true = J_true, J_use = J_use, N_cell_true = N_cell_true, J_age = length(unique(dat$age)), 
    J_eth = length(unique(dat$eth)), J_edu = length(unique(dat$edu)), J_age_eth = length(unique(dat$age)) * length(unique(dat$eth)), 
    J_age_edu = length(unique(dat$age)) * length(unique(dat$edu)), J_eth_edu = length(unique(dat$eth)) * length(unique(dat$edu)), 
    J_age_eth_edu = length(unique(dat$age)) * length(unique(dat$eth)) * length(unique(dat$edu)))


S.compile_cell_sp <- stan(file = "stan/mrpweights-3var.stan", data = stan.data_cell, iter = 2, chains = 1)

S.compile_cell_1 <- stan(file = "stan/mrpweights-3var-iid.stan", data = stan.data_cell, iter = 2, chains = 1)
###------repeated sampling process------###
R <- 200

# output files
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

for (r in 1:R) {
    set.seed(20150213 + r)
    
    
    ### sampling
    I <- (runif(N) <= sel_prob)
    
    dat <- data.frame(Y = Y[I], age = acs_ad$age_dc[I], eth = acs_ad$race_dc[I], edu = acs_ad$educat[I])
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
    
    y_cell <- aggregate(. ~ cell_id, data = dat, mean)$Y
    ss_cell <- 0
    for (j in 1:J) {
        ss_cell <- ss_cell + sum((dat$Y[cell_id == J_use[j]] - y_cell[j])^2)
    }
    
    ###-----------------STAN with structural prior--------------------------###
    
    stan.data_cell <- list(n = nrow(dat), q = q, J = J, n_cell = n_cell, y_cell = y_cell, ss_cell = ss_cell, cell_str = cell_str, 
        N_cell = N_cell, J_true = J_true, J_use = J_use, N_cell_true = N_cell_true, J_age = length(unique(dat$age)), 
        J_eth = length(unique(dat$eth)), J_edu = length(unique(dat$edu)), J_age_eth = length(unique(dat$age)) * 
            length(unique(dat$eth)), J_age_edu = length(unique(dat$age)) * length(unique(dat$edu)), J_eth_edu = length(unique(dat$eth)) * 
            length(unique(dat$edu)), J_age_eth_edu = length(unique(dat$age)) * length(unique(dat$eth)) * length(unique(dat$edu)))
    
    stan.seed <- round(runif(1, 0, 9999999))
    n.chains <- 3
    registerDoMC(n.chains)
    st <- system.time(sflist <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
        S <- stan(fit = S.compile_cell_sp, data = stan.data_cell, iter = 500, chains = 3, seed = stan.seed, chain_id = i.cores)
        return(S)
    })[3]
    S <- sflist2stanfit(sflist)
    
    output <- extract(S, permuted = TRUE)
    
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
    bias_mu_popcell[r, ] <- apply(output$mu_cell_pred, 2, mean) - mu_cell_true
    sd_mu_popcell[r, ] <- apply(output$mu_cell_pred, 2, sd)
    cr_mu_popcell[r, ] <- as.numeric(apply(output$mu_cell_pred, 2, quantile, 0.025) <= mu_cell_true & mu_cell_true <= 
        apply(output$mu_cell_pred, 2, quantile, 0.975))
    
    ### sampled cell mean
    bias_mu_cell[r, J_use] <- apply(output$mu_cell, 2, mean) - mu_cell_use
    sd_mu_cell[r, J_use] <- apply(output$mu_cell, 2, sd)
    cr_mu_cell[r, J_use] <- as.numeric(apply(output$mu_cell, 2, quantile, 0.025) <= mu_cell_use & mu_cell_use <= 
        apply(output$mu_cell, 2, quantile, 0.975))
    
    
    ### weights
    w_unit <- rep(0, n)
    for (i in 1:n) {
        w_unit[i] <- apply(output$w_new, 2, mean)[J_use == cell_id[i]]
    }
    w_unit <- w_unit/mean(w_unit)
    
    wght_sd_rt[r, ] <- c(sd(w_unit), max(w_unit)/min(w_unit))
    
    mu_st1 <- mean(w_unit * dat$Y)
    bias_mu_st[r] <- mu_st1 - mean(Y)
    sd_mu_st[r] <- sqrt(sum(w_unit^2 * var(dat$Y)))/n
    
    cr_mu_st[r] <- as.numeric(mu_st1 - 1.96 * sd_mu_st[r] <= mean(Y) & mean(Y) <= mu_st1 + 1.96 * sd_mu_st[r])
    
    ###-----------------STAN with independent prior--------------------------###
    
    registerDoMC(n.chains)
    st <- system.time(sflist1 <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
        S1 <- stan(fit = S.compile_cell_1, data = stan.data_cell, iter = 500, chains = 3, seed = stan.seed, chain_id = i.cores)
        return(S1)
    })[3]
    S1 <- sflist2stanfit(sflist1)
    
    output_iid <- extract(S1, permuted = TRUE)
    
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
    bias_mu_popcell_iid[r, ] <- apply(output_iid$mu_cell_pred, 2, mean) - mu_cell_true
    sd_mu_popcell_iid[r, ] <- apply(output_iid$mu_cell_pred, 2, sd)
    cr_mu_popcell_iid[r, ] <- as.numeric(apply(output_iid$mu_cell_pred, 2, quantile, 0.025) <= mu_cell_true & 
        mu_cell_true <= apply(output_iid$mu_cell_pred, 2, quantile, 0.975))
    
    ### sampled cell mean
    bias_mu_cell_iid[r, J_use] <- apply(output_iid$mu_cell, 2, mean) - mu_cell_use
    sd_mu_cell_iid[r, J_use] <- apply(output_iid$mu_cell, 2, sd)
    cr_mu_cell_iid[r, J_use] <- as.numeric(apply(output_iid$mu_cell, 2, quantile, 0.025) <= mu_cell_use & mu_cell_use <= 
        apply(output_iid$mu_cell, 2, quantile, 0.975))
    
    ### weights
    w_unit_iid <- rep(0, n)
    for (i in 1:n) {
        w_unit_iid[i] <- apply(output_iid$w_new, 2, mean)[J_use == cell_id[i]]
    }
    w_unit_iid <- w_unit_iid/mean(w_unit_iid)
    wght_sd_rt_iid[r, ] <- c(sd(w_unit_iid), max(w_unit_iid)/min(w_unit_iid))
    
    mu_id1 <- mean(w_unit_iid * dat$Y)
    bias_mu_id[r] <- mu_id1 - mean(Y)
    sd_mu_id[r] <- sqrt(sum(w_unit^2 * var(dat$Y)))/n
    cr_mu_id[r] <- as.numeric(mu_id1 - 1.96 * sd_mu_id[r] <= mean(Y) & mean(Y) <= mu_id1 + 1.96 * sd_mu_id[r])
    
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
    
    for (v in 1:q) {
        for (l in 1:l_v[v + 1]) {
            cell_index <- (1:J_true)[cell_str[, v] == l]
            id_index <- cell_id %in% (1:J_true)[cell_str[, v] == l]
            mar_true <- sum(mu_cell_true[cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
            # model prediction
            for (s in 1:dim(output$mu_cell_pred)[1]) {
                st_est_sm[s] <- sum(output$mu_cell_pred[s, cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
            }
            # st_est_sm<-apply(output$mu_cell_pred[,cell_index] *
            # N_cell_true[cell_index],1,sum)/sum(N_cell_true[cell_index])
            bias_sub_st[r, l + sum(l_v[1:v])] <- mean(st_est_sm) - mar_true
            sd_sub_st[r, l + sum(l_v[1:v])] <- sd(st_est_sm)
            if (quantile(st_est_sm, 0.025) <= mar_true & mar_true <= quantile(st_est_sm, 0.975)) {
                cr_sub_st[r, l + sum(l_v[1:v])] <- 1
            }
            
            # independent prior
            for (s in 1:dim(output$mu_cell_pred)[1]) {
                iid_est_sm[s] <- sum(output_iid$mu_cell_pred[s, cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
            }
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
    
    # model prediction
    for (s in 1:dim(output$mu_cell_pred)[1]) {
        st_est_sm[s] <- sum(output$mu_cell_pred[s, cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
    }
    bias_sub_st_int[r] <- mean(st_est_sm) - mar_true
    sd_sub_st_int[r] <- sd(st_est_sm)
    if (quantile(st_est_sm, 0.025) <= mar_true & mar_true <= quantile(st_est_sm, 0.975)) {
        cr_sub_st_int[r] <- 1
    }
    
    # independent prior
    for (s in 1:dim(output$mu_cell_pred)[1]) {
        iid_est_sm[s] <- sum(output_iid$mu_cell_pred[s, cell_index] * N_cell_true[cell_index])/sum(N_cell_true[cell_index])
    }
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

###------------plots------------### 
over_pred_mean <- data.frame(cbind(c(mean(bias_mu_pred), sqrt(mean(bias_mu_pred^2)), mean(sd_mu_pred), mean(cr_mu_pred)), 
    c(mean(bias_mu_pred_iid), sqrt(mean(bias_mu_pred_iid^2)), mean(sd_mu_pred_iid), mean(cr_mu_pred_iid))))


over_wgt_mean <- data.frame(cbind(c(mean(bias_mu_st), sqrt(mean(bias_mu_st^2)), mean(sd_mu_st), mean(cr_mu_st)), 
    c(mean(bias_mu_id), sqrt(mean(bias_mu_id^2)), mean(sd_mu_id), mean(cr_mu_id)), c(mean(bias_mu_ps), sqrt(mean(bias_mu_ps^2)), 
        mean(sd_mu_ps), mean(cr_mu_ps)), c(mean(bias_mu_rake), sqrt(mean(bias_mu_rake^2)), mean(sd_mu_rake), mean(cr_mu_rake)), 
    c(mean(bias_mu_ips), sqrt(mean(bias_mu_ips^2)), mean(sd_mu_ips), mean(cr_mu_ips))))

mar_pred_mean <- data.frame(cbind(c(apply(bias_sub_st, 2, mean), sqrt(apply(bias_sub_st^2, 2, mean)), apply(sd_sub_st, 
    2, mean), apply(cr_sub_st, 2, mean)), c(apply(bias_sub_iid, 2, mean), sqrt(apply(bias_sub_iid^2, 2, mean)), 
    apply(sd_sub_iid, 2, mean), apply(cr_sub_iid, 2, mean))))


mar_wgt_mean <- data.frame(cbind(c(apply(bias_sub_st_wt, 2, mean), sqrt(apply(bias_sub_st_wt^2, 2, mean)), apply(sd_sub_st_wt, 
    2, mean), apply(cr_sub_st_wt, 2, mean)), c(apply(bias_sub_iid_wt, 2, mean), sqrt(apply(bias_sub_iid_wt^2, 
    2, mean)), apply(sd_sub_iid_wt, 2, mean), apply(cr_sub_iid_wt, 2, mean)), c(apply(bias_sub_ps_wt, 2, mean), 
    sqrt(apply(bias_sub_ps_wt^2, 2, mean)), apply(sd_sub_ps_wt, 2, mean), apply(cr_sub_ps_wt, 2, mean)), c(apply(bias_sub_rake_wt, 
    2, mean), sqrt(apply(bias_sub_rake_wt^2, 2, mean)), apply(sd_sub_rake_wt, 2, mean), apply(cr_sub_rake_wt, 
    2, mean)), c(apply(bias_sub_ips_wt, 2, mean), sqrt(apply(bias_sub_ips_wt^2, 2, mean)), apply(sd_sub_ips_wt, 
    2, mean), apply(cr_sub_ips_wt, 2, mean))))


int_pred_mean <- data.frame(cbind(c(mean(bias_sub_st_int), sqrt(mean(bias_sub_st_int^2)), mean(sd_sub_st_int), 
    mean(cr_sub_st_int)), c(mean(bias_sub_iid_int), sqrt(mean(bias_sub_iid_int^2)), mean(sd_sub_iid_int), mean(cr_sub_iid_int))))


int_wgt_mean <- data.frame(cbind(c(mean(bias_sub_st_wt_int), sqrt(mean(bias_sub_st_wt_int^2)), mean(sd_sub_st_wt_int), 
    mean(cr_sub_st_wt_int)), c(mean(bias_sub_iid_wt_int), sqrt(mean(bias_sub_iid_wt_int^2)), mean(sd_sub_iid_wt_int), 
    mean(cr_sub_iid_wt_int)), c(mean(bias_sub_ps_wt_int), sqrt(mean(bias_sub_ps_wt_int^2)), mean(sd_sub_ps_wt_int), 
    mean(cr_sub_ps_wt_int)), c(mean(bias_sub_rake_wt_int), sqrt(mean(bias_sub_rake_wt_int^2)), mean(sd_sub_rake_wt_int), 
    mean(cr_sub_rake_wt_int)), c(mean(bias_sub_ips_wt_int), sqrt(mean(bias_sub_ips_wt_int^2)), mean(sd_sub_ips_wt_int), 
    mean(cr_sub_ips_wt_int))))


### bias
bias_out <- data.frame(cbind(c("overall", "non-white young", "age:18-34", "age:35-44", "age:45-54", "age:55-64", 
    "age:65+", "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", 
    ">=col"), rbind(c(abs(over_pred_mean[1, ]), abs(over_wgt_mean[1, ])), c(abs(int_pred_mean[1, ]), abs(int_wgt_mean[1, 
    ])), cbind(abs(mar_pred_mean[1:sum(l_v), ]), abs(mar_wgt_mean[1:sum(l_v), ])))))

colnames(bias_out) <- c("Quantity", "Str-P", "Ind-P", "Str-W", "Ind-W", "PS-W", "Rake-W", "IP-W")
bias_out$Quantity <- factor(bias_out$Quantity, levels = c("age:18-34", "age:35-44", "age:45-54", "age:55-64", 
    "age:65+", "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", 
    ">=col", "non-white young", "overall"))
bias_out.m <- melt(bias_out)

ggplot(bias_out.m, aes(variable, Quantity)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(name = "abs.Bias", 
    low = "white", high = "steelblue") + labs(x = "", y = "") + scale_x_discrete() + scale_y_discrete() + theme_bw() + 
    theme(axis.line = element_line(colour = "black"), axis.text = element_text(size = 14), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())

ggsave("plot/var3_bias_case1.pdf")

### rmse
rmse_out <- data.frame(cbind(c("overall", "non-white young", "age:18-34", "age:35-44", "age:45-54", "age:55-64", 
    "age:65+", "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", 
    ">=col"), rbind(c(over_pred_mean[2, ], over_wgt_mean[2, ]), c(int_pred_mean[2, ], int_wgt_mean[2, ]), cbind(mar_pred_mean[sum(l_v) + 
    1:sum(l_v), ], mar_wgt_mean[sum(l_v) + 1:sum(l_v), ]))))

colnames(rmse_out) <- c("Quantity", "Str-P", "Ind-P", "Str-W", "Ind-W", "PS-W", "Rake-W", "IP-W")
rmse_out$Quantity <- factor(rmse_out$Quantity, levels = c("age:18-34", "age:35-44", "age:45-54", "age:55-64", 
    "age:65+", "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", 
    ">=col", "non-white young", "overall"))
rmse_out.m <- melt(rmse_out)

ggplot(rmse_out.m, aes(variable, Quantity)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(name = "RMSE", 
    low = "white", high = "steelblue") + labs(x = "", y = "") + scale_x_discrete() + scale_y_discrete() + theme_bw() + 
    theme(axis.line = element_line(colour = "black"), axis.text = element_text(size = 14), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())

ggsave("plot/var3_rmse_case1.pdf")

### se
se_out <- data.frame(cbind(c("overall", "non-white young", "age:18-34", "age:35-44", "age:45-54", "age:55-64", 
    "age:65+", "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", 
    ">=col"), rbind(c(over_pred_mean[3, ], over_wgt_mean[3, ]), c(int_pred_mean[3, ], int_wgt_mean[3, ]), cbind(mar_pred_mean[2 * 
    sum(l_v) + 1:sum(l_v), ], mar_wgt_mean[2 * sum(l_v) + 1:sum(l_v), ]))))

colnames(se_out) <- c("Quantity", "Str-P", "Ind-P", "Str-W", "Ind-W", "PS-W", "Rake-W", "IP-W")
se_out$Quantity <- factor(se_out$Quantity, levels = c("age:18-34", "age:35-44", "age:45-54", "age:55-64", "age:65+", 
    "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", ">=col", 
    "non-white young", "overall"))
se_out.m <- melt(se_out)
ggplot(se_out.m, aes(variable, Quantity)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(name = "Ave.SD", 
    low = "white", high = "steelblue") + labs(x = "", y = "") + scale_x_discrete() + scale_y_discrete() + theme_bw() + 
    theme(axis.line = element_line(colour = "black"), axis.text = element_text(size = 14), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())

ggsave("plot/var3_se_case1.pdf")

cr_out <- data.frame(cbind(c("overall", "non-white young", "age:18-34", "age:35-44", "age:45-54", "age:55-64", 
    "age:65+", "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", 
    ">=col"), rbind(c(over_pred_mean[4, ], over_wgt_mean[4, ]), c(int_pred_mean[4, ], int_wgt_mean[4, ]), cbind(mar_pred_mean[3 * 
    sum(l_v) + 1:sum(l_v), ], mar_wgt_mean[3 * sum(l_v) + 1:sum(l_v), ]))))

colnames(cr_out) <- c("Quantity", "Str-P", "Ind-P", "Str-W", "Ind-W", "PS-W", "Rake-W", "IP-W")
cr_out$Quantity <- factor(cr_out$Quantity, levels = c("age:18-34", "age:35-44", "age:45-54", "age:55-64", "age:65+", 
    "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", ">=col", 
    "non-white young", "overall"))
cr_out.m <- melt(cr_out)

ggplot(cr_out.m, aes(variable, Quantity)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(name = "Coverage", 
    low = "steelblue", high = "white") + labs(x = "", y = "") + scale_x_discrete() + scale_y_discrete() + theme_bw() + 
    theme(axis.line = element_line(colour = "black"), axis.text = element_text(size = 14), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())

ggsave("plot/var3_cr_case1.pdf")
