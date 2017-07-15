###---------------MRP to Construct Survey Weights--------------###
### Author: YS Latest Edit date: 06/07/2017 clear
remove(list = objects())

#----------required packages-----------------#
require(foreign)
require(rstan)
require(survey)
require(ggplot2)
require(gridExtra)
library(xtable)
library(directlabels)
library(reshape2)

set.seed(20150213)

#----------data simulation-----------------#
data <- read.dta("weighting_code/data/SRBIandAGENCYbaselinew1w2w3datawithwghts072114.dta")
SRBIdata <- data[data$sample == "SRBI", ]
# poorhealth health anyhard hardscl bestlife=qd7 la var bestlife 'Life Satisfaction, 10=best possibl
# table(SRBIdata[, names(SRBIdata) == 'bestlife']) table(SRBIdata[,names(SRBIdata)=='hardcount2'])
# table(SRBIdata[,names(SRBIdata)=='freqhosp'])
# table(SRBIdata[,names(SRBIdata)=='bestlife'],SRBIdata[,names(SRBIdata)=='health'])
# table(SRBIdata[,names(SRBIdata)=='bestlife'],SRBIdata[,names(SRBIdata)=='anyhard'])
# chisq.test(SRBIdata$bestlife,SRBIdata$anyhard)

Y_sys <- as.numeric(SRBIdata[, names(SRBIdata) == "bestlife"])
Y_sys[is.na(Y_sys)] <- sample(Y_sys[!is.na(Y_sys)], sum(is.na(Y_sys)), replace = T)
# hist(Y_sys) ACS data
acs_pop <- read.dta("weighting_code/data/acs_nyc_2011_wpov1.dta", convert.factors = FALSE)
acs_ad <- acs_pop[as.numeric(acs_pop$age) >= 18, ]
# age
age_tr <- acs_ad$age  #
acs_ad$age_dc <- age_tr  #discretization
acs_ad$age_dc[age_tr <= 34] <- 1  #18-34
acs_ad$age_dc[age_tr <= 64 & age_tr > 34] <- 2  #35-64
acs_ad$age_dc[age_tr >= 64] <- 3  #65+

age_tr <- acs_ad$age  #at least 18
acs_ad$age_dc <- age_tr  #discretization
acs_ad$age_dc[age_tr <= 34] <- 1  #18-34
acs_ad$age_dc[age_tr <= 44 & age_tr > 34] <- 2
acs_ad$age_dc[age_tr <= 54 & age_tr > 44] <- 3
acs_ad$age_dc[age_tr <= 64 & age_tr > 54] <- 4
acs_ad$age_dc[age_tr > 64] <- 5

SRBIdata$age_dc <- SRBIdata$age
SRBIdata$age_dc[SRBIdata$age <= 34] <- 1
SRBIdata$age_dc[SRBIdata$age <= 64 & SRBIdata$age > 34] <- 2
SRBIdata$age_dc[SRBIdata$age > 64] <- 3

SRBIdata$age_dc <- SRBIdata$age
SRBIdata$age_dc[SRBIdata$age <= 34] <- 1
SRBIdata$age_dc[SRBIdata$age <= 44 & SRBIdata$age > 34] <- 2
SRBIdata$age_dc[SRBIdata$age <= 54 & SRBIdata$age > 44] <- 3
SRBIdata$age_dc[SRBIdata$age <= 64 & SRBIdata$age > 54] <- 4
SRBIdata$age_dc[SRBIdata$age > 64] <- 5

# sex
acs_ad$sex <- as.numeric(acs_ad$sex)
SRBIdata$sex <- as.numeric(as.factor(SRBIdata$r_gender))

# race race 1 '1 White Non-Hispanic' 2 '2 Black Non-Hispanic' 3 '3 Asian' 4 '4 Other' 5 '5 Hispanic'
acs_ad$race_dc <- acs_ad$racex
SRBIdata$race_dc <- SRBIdata$race

# educat 4 recode qi5 (1/2=1) (3/4=2) (5/6=3) (7/8=4) (98/99=.), gen(educat)

# poverty gap: 1 Under 50% 2 50-100% 3 100-200% 4 200-300% 5 300%+
acs_ad$opmres_x <- 1
# acs_ad$poverty
acs_ad$opmres_x[acs_ad$poverty <= 100 & acs_ad$poverty > 50] <- 2
acs_ad$opmres_x[acs_ad$poverty <= 200 & acs_ad$poverty > 100] <- 3
acs_ad$opmres_x[acs_ad$poverty <= 300 & acs_ad$poverty > 200] <- 4
acs_ad$opmres_x[acs_ad$poverty > 300] <- 5

SRBIdata$opmres_x <- 1
# (1/100=1) (101-200=2) (201/501=3
SRBIdata$opmres_x[SRBIdata$povgap == "2 50-100%"] <- 2
SRBIdata$opmres_x[SRBIdata$povgap == "3 100-200%"] <- 3
SRBIdata$opmres_x[SRBIdata$povgap == "4 200-300%"] <- 4
SRBIdata$opmres_x[SRBIdata$povgap == "5 300%+"] <- 5

# eldx
acs_ad$eldx_ca <- acs_ad$eldx  # 0 1 2+
acs_ad$eldx_ca[acs_ad$eldx > 1] <- 2
acs_ad$eldx_ca <- acs_ad$eldx_ca + 1  #change to positive integer

SRBIdata$eldx_ca <- SRBIdata$eldx
SRBIdata$eldx_ca[SRBIdata$eldx > 1] <- 2
SRBIdata$eldx_ca <- SRBIdata$eldx_ca + 1

# childx 0 1 2 3
acs_ad$childx_ca <- acs_ad$childx
acs_ad$childx_ca[acs_ad$childx_ca > 2] <- 3
acs_ad$childx_ca <- acs_ad$childx_ca + 1

SRBIdata$childx_ca <- SRBIdata$childx
SRBIdata$childx_ca[SRBIdata$childx_ca > 2] <- 3
SRBIdata$childx_ca <- SRBIdata$childx_ca + 1

# wax 0 1 2 acs_ad$wax_ca<-acs_ad$wax acs_ad$wax_ca[acs_ad$wax>1]<-2

# personx 1-4
acs_ad$personx_ca <- acs_ad$personx
acs_ad$personx_ca[acs_ad$personx > 4] <- 4
SRBIdata$personx_ca <- SRBIdata$personx
SRBIdata$personx_ca[SRBIdata$personx_ca > 4] <- 4

J_age <- length(unique(acs_ad$age_dc))
J_eth <- length(unique(acs_ad$race_dc))
J_edu <- length(unique(acs_ad$educat))
J_sex <- length(unique(acs_ad$sex))
J_inc <- length(unique(acs_ad$opmres_x))
J_eld <- length(unique(acs_ad$eldx_ca))
J_cld <- length(unique(acs_ad$childx_ca))
J_ps <- length(unique(acs_ad$personx_ca))

###--------Four variable case: age, race, edu and inc------------###
q <- 4
acs_ds_ad <- svydesign(id = ~1, weights = ~perwt, data = acs_ad)
acs_N <- as.numeric(svytable(~age_dc + race_dc + educat + opmres_x, acs_ds_ad))

N <- dim(acs_ad)[1]
N_0 <- sum(acs_N)

dat <- data.frame(Y = Y_sys, age = SRBIdata$age_dc, eth = SRBIdata$race_dc, edu = SRBIdata$educat, sex = SRBIdata$sex, 
                  inc = SRBIdata$opmres_x, eldx = SRBIdata$eldx_ca, cldx = SRBIdata$childx_ca, psx = SRBIdata$personx_ca)
n <- dim(dat)[1]  #sample size

as.numeric(table(dat$age, dat$eth, dat$edu, dat$inc))

pop_cell_id <- rep(0, N)
cell_id <- rep(0, n)
J_sup <- J_age * J_eth * J_edu * J_inc
cell_str <- matrix(0, J_sup, q)
j <- 0

for (i4 in 1:J_inc) {
  for (i3 in 1:J_edu) {
    for (i2 in 1:J_eth) {
      for (i1 in 1:J_age) {
        j <- (i4 - 1) * J_edu * J_eth * J_age + (i3 - 1) * J_eth * J_age + (i2 - 1) * J_age + i1
        pop_cell_id[acs_ad$age_dc == i1 & acs_ad$race_dc == i2 & acs_ad$educat == i3 & acs_ad$opmres_x == 
                      i4] <- j
        cell_id[dat$age == i1 & dat$eth == i2 & dat$edu == i3 & dat$inc == i4] <- j
        cell_str[j, ] <- c(i1, i2, i3, i4)
      }
    }
  }
}

N_cell_acs <- acs_N[as.numeric(names(table(pop_cell_id)))]
J_true <- length(names(table(pop_cell_id)))
#-----------computation--------------#
J <- length(unique(cell_id))
J_use <- as.numeric(names(table(cell_id)))
n_cell <- as.numeric(table(cell_id))
J_pop <- as.numeric(names(table(pop_cell_id)))
J_pop_acs <- as.numeric(names(table(pop_cell_id)))  #ACS occupiled cell

N_cell_true <- rep(0, J_true)

N_cell <- rep(0, J)
for (j in 1:J) {
  if (J_use[j] %in% J_pop_acs) {
    N_cell[j] <- N_cell_acs[J_pop_acs == J_use[j]]
  } else {
    N_cell[j] <- n_cell[as.numeric(names(table(cell_id))) == J_use[j]]
  }
}
for (j in 1:J_true) {
  if (J_pop[j] %in% J_pop_acs) {
    N_cell_true[j] <- N_cell_acs[J_pop_acs == J_pop[j]]
  } else {
    N_cell_true[j] <- n_cell[as.numeric(names(table(cell_id))) == J_pop[j]]
  }
}
# #rank according to cell_id
###------------###
y_cell <- aggregate(. ~ cell_id, data = dat, mean)$Y
ss_cell <- 0
for (j in 1:J) {
  ss_cell <- ss_cell + sum((dat$Y[cell_id == J_use[j]] - y_cell[j])^2)
}

###-----------------STAN with structural prior--------------------------###

stan.data_cell <- list(n = nrow(dat), q = q, J = J, n_cell = n_cell, y_cell = y_cell, ss_cell = ss_cell, cell_str = cell_str, 
                       N_cell = N_cell, J_true = J_true, J_use = J_use, J_pop = J_pop, J_sup = J_sup, N_cell_true = N_cell_true, 
                       J_age = J_age, J_eth = J_eth, J_edu = J_edu, J_inc = J_inc, J_age_eth = J_age * J_eth, J_age_edu = J_age * 
                         J_edu, J_eth_edu = J_eth * J_edu, J_eth_inc = J_eth * J_inc, J_age_inc = J_age * J_inc, J_age_eth_edu = J_age * 
                         J_eth * J_edu, J_age_eth_inc = J_age * J_eth * J_inc)


S.compile_cell_4var <- stan_model(file = "weighting_code/stan/mrpweights-4var.stan")

S_4var <- sampling(object = S.compile_cell_4var, data = stan.data_cell, iter = 8000, chains = 4, seed = 1234, cores =4)

output <- extract(S_4var, permuted = TRUE)

### model-based weights
w_unit <- rep(0, n)
tw <- apply(output$w_new, 2, mean)
for (i in 1:n) {
  w_unit[i] <- tw[J_use == cell_id[i]]
}
w_unit <- w_unit/mean(w_unit)

st.dat.design <- svydesign(id = ~1, data = dat, weights = w_unit)
svymean(~Y, st.dat.design)

###------PS weighting------###
w_ps <- rep(1, n)
for (i in 1:n) {
  w_ps[i] <- (N_cell/n_cell)[J_use == cell_id[i]]
}
w_ps <- w_ps/mean(w_ps)

ps.dat.design <- svydesign(id = ~1, data = dat, weights = w_ps)

svymean(~Y, ps.dat.design)

###------raking estimator------###
dat.design <- svydesign(id = ~1, data = dat)

pop.age <- data.frame(1:J_age, Freq = as.numeric(svytable(~age_dc, acs_ds_ad)))
names(pop.age) <- c("age", "Freq")
pop.eth <- data.frame(1:J_eth, Freq = as.numeric(svytable(~race_dc, acs_ds_ad)))
names(pop.eth) <- c("eth", "Freq")
pop.edu <- data.frame(1:J_edu, Freq = as.numeric(svytable(~educat, acs_ds_ad)))
names(pop.edu) <- c("edu", "Freq")
pop.inc <- data.frame(1:J_inc, Freq = as.numeric(svytable(~opmres_x, acs_ds_ad)))
names(pop.inc) <- c("inc", "Freq")

dat_rake <- rake(dat.design, list(~age, ~eth, ~edu, ~inc), list(pop.age, pop.eth, pop.edu, pop.inc), control = list(maxit = 10, 
                                                                                                                    epsilon = 1, verbose = FALSE))

w_rake <- weights(dat_rake)/mean(weights(dat_rake))
rake.dat.design <- svydesign(id = ~1, data = dat, weights = w_rake)
svymean(~Y, rake.dat.design)

# poorhealth health anyhard hardscl
st.dat.design <- svydesign(id = ~1, data = SRBIdata, weights = w_unit)
ps.dat.design <- svydesign(id = ~1, data = SRBIdata, weights = w_ps)
rake.dat.design <- svydesign(id = ~1, data = SRBIdata, weights = w_rake)


p.d <- svytable(~age_dc, acs_ds_ad)/sum(svytable(~age_dc, acs_ds_ad))
s.d <- svytable(~age, st.dat.design)/n
sum(p.d * log(p.d/s.d))
sqrt(sum((p.d - s.d)^2))

s.d.ps <- svytable(~age, ps.dat.design)/n
sum(p.d * log(p.d/s.d.ps))
sqrt(sum((p.d - s.d.ps)^2))

s.d.rake <- svytable(~age, rake.dat.design)/n
sum(p.d * log(p.d/s.d.rake))

p.d <- svytable(~educat + age_dc, acs_ds_ad)/sum(svytable(~educat + age_dc, acs_ds_ad))
s.d <- svytable(~edu + age, st.dat.design)/n
s.d.ps <- svytable(~edu + age, ps.dat.design)/n
s.d.rake <- svytable(~edu + age, rake.dat.design)/n

print(xtable(data.frame(cbind(sqrt(sum((p.d - s.d)^2)), sqrt(sum((p.d - s.d.ps)^2)), sqrt(sum((p.d - s.d.rake)^2)))), 
             digits = 3))

weights <- cbind(c(w_unit, w_rake, w_ps), c(rep("Str-W", n), rep("Rake-W", n), rep("PS-W", n)))
weights <- data.frame(weights)
names(weights) <- c("wt", "Method")
weights$wt <- log(as.numeric(as.character(weights$wt)))
weights$Method <- factor(weights$Method, levels = c("Str-W", "Rake-W", "PS-W"))

direct.label(ggplot(weights, aes(x = wt, group = Method)) + geom_density(aes(color = Method)) + theme_bw() + scale_x_continuous(name = "Distributions of log(weights) in the LSW", 
                                                                                                                                expand = c(0, 0)) + scale_y_continuous(name = "", expand = c(0, 0), limits = c(0, 1.35)) + theme(axis.line.x = element_line(colour = "black"), 
                                                                                                                                                                                                                                 axis.line.y = element_blank(), legend.position = "", legend.title = element_blank(), axis.text.x = element_text(size = 14), 
                                                                                                                                                                                                                                 axis.title = element_text(size = 16), axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
                                                                                                                                                                                                                                 panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()))
ggsave("plot/weight-lsw.pdf")

weights_s <- cbind(c(w_unit/sum(w_unit), w_rake/sum(w_rake), w_ps/sum(w_ps), rep(1/n, n)), c(rep("Str-W", n), 
                                                                                             rep("Rake-W", n), rep("PS-W", n), rep("Sample", n)))

weights_y <- data.frame(cbind(weights_s, dat$Y))
names(weights_y) <- c("w", "Method", "Y")
weights_y$w <- as.numeric(as.character(weights_y$w))
weights_y$Y <- as.numeric(as.character(weights_y$Y))
weights_y$Method <- factor(weights_y$Method, levels = c("Str-W", "Rake-W", "PS-W", "Sample"))


ggplot(weights_y, aes(x = Y, weights = w, group = Method)) + geom_density(aes(color = Method)) + theme_bw() + 
  scale_x_continuous(name = "Weighted distribution of life satisfaction score in the LSW") + scale_y_continuous(name = "", 
                                                                                                                expand = c(0, 0)) + theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_blank(), legend.position = c(0.2, 
                                                                                                                                                                                                                                           0.75), legend.title = element_blank(), axis.text.x = element_text(size = 15), axis.title = element_text(size = 16), 
                                                                                                                                          axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                                                                                          panel.border = element_blank(), panel.background = element_blank())
ggsave("plot/weighted-lsw-density.pdf")

over_mean_wgt <- data.frame(cbind(c(sd(w_unit), max(w_unit)/min(w_unit), mean(output$theta_pred), sd(output$theta_pred)), 
                                  c(sd(w_unit), max(w_unit)/min(w_unit), sum(w_unit * dat$Y)/sum(w_unit), sqrt(sum(w_unit^2 * var(dat$Y)))/n), 
                                  c(sd(w_rake), max(w_rake)/min(w_rake), sum(w_rake * dat$Y)/sum(w_rake), sqrt(sum(w_rake^2 * var(dat$Y)))/n), 
                                  c(sd(w_ps), max(w_ps)/min(w_ps), sum(w_ps * dat$Y)/sum(w_ps), sqrt(sum(w_ps^2 * var(dat$Y)))/n)))
rownames(over_mean_wgt) <- c("SD.W", "Max/Min.W", "Est", "SE")
colnames(over_mean_wgt) <- c("Str-P", "Str-W", "Rake-W", "PS-W")

print(xtable(over_mean_wgt, digits = 3))

### marginal means sub domain
l_v <- c(0, J_age, J_eth, J_edu, J_inc)
est_sub_st <- rep(0, sum(l_v))
sd_sub_st <- rep(0, sum(l_v))
est_sub_st_wt <- rep(0, sum(l_v))
sd_sub_st_wt <- rep(0, sum(l_v))
est_sub_st_wt2 <- rep(0, sum(l_v))
sd_sub_st_wt2 <- rep(0, sum(l_v))
est_sub_ps_wt <- rep(0, sum(l_v))
sd_sub_ps_wt <- rep(0, sum(l_v))
est_sub_ips_wt <- rep(0, sum(l_v))
sd_sub_ips_wt <- rep(0, sum(l_v))
est_sub_rake_wt <- rep(0, sum(l_v))
sd_sub_rake_wt <- rep(0, sum(l_v))
st_est_sm <- rep(0, dim(output$mu_cell_pred)[1])
iid_est_sm <- rep(0, dim(output$mu_cell_pred)[1])

for (v in 1:q) {
  for (l in 1:l_v[v + 1]) {
    cell_index <- (1:J_sup)[cell_str[, v] == l]
    id_index <- cell_id %in% (1:J_sup)[cell_str[, v] == l]
    for (s in 1:dim(output$mu_cell_pred)[1]) {
      st_est_sm[s] <- sum(output$mu_cell_pred[s, J_pop %in% cell_index] * N_cell_true[J_pop %in% cell_index])/sum(N_cell_true[J_pop %in% 
                                                                                                                                cell_index])
    }
    
    est_sub_st[l + sum(l_v[1:v])] <- mean(st_est_sm)
    sd_sub_st[l + sum(l_v[1:v])] <- sd(st_est_sm)
    
    # model-based weights under st prior
    est_st_wt <- sum(w_unit[id_index] * dat$Y[id_index])/sum(w_unit[id_index])
    est_sub_st_wt[l + sum(l_v[1:v])] <- est_st_wt
    sd_sub_st_wt[l + sum(l_v[1:v])] <- sqrt(sum(w_unit[id_index]^2 * var(dat$Y[id_index])))/sum(w_unit[id_index])
    
    # ps weights
    est_ps_wt <- sum(w_ps[id_index] * dat$Y[id_index])/sum(w_ps[id_index])
    est_sub_ps_wt[l + sum(l_v[1:v])] <- est_ps_wt
    sd_sub_ps_wt[l + sum(l_v[1:v])] <- sqrt(sum(w_ps[id_index]^2 * var(dat$Y[id_index])))/sum(w_ps[id_index])
    
    # rake weights
    est_rake_wt <- sum(w_rake[id_index] * dat$Y[id_index])/sum(w_rake[id_index])
    est_sub_rake_wt[l + sum(l_v[1:v])] <- est_rake_wt
    sd_sub_rake_wt[l + sum(l_v[1:v])] <- sqrt(sum(w_rake[id_index]^2 * var(dat$Y[id_index])))/sum(w_rake[id_index])
  }
}

mar_pred_mean <- c(est_sub_st, sd_sub_st)
mar_wgt_mean <- data.frame(cbind(c(est_sub_st_wt, sd_sub_st_wt), c(est_sub_ps_wt, sd_sub_ps_wt), c(est_sub_rake_wt, 
                                                                                                   sd_sub_rake_wt)))

est_out <- data.frame(cbind(c("age:18-34", "age:35-44", "age:45-54", "age:55-64", "age:65+", "whi&non-Hisp", "blac&non-Hisp", 
                              "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", ">=col", "pov-gap <50%", "pov-gap50-100%", 
                              "pov-gap100-200%", "pov-gap200-300%", "pov-gap300%+"), mar_pred_mean[1:sum(l_v)], mar_wgt_mean[1:sum(l_v), 
                                                                                                                             ]))
colnames(est_out) <- c("Quantity", "Str-P", "Str-W", "PS-W", "Rake-W")
est_out$Quantity <- factor(est_out$Quantity, levels = c("age:18-34", "age:35-44", "age:45-54", "age:55-64", "age:65+", 
                                                        "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", ">=col", 
                                                        "pov-gap <50%", "pov-gap50-100%", "pov-gap100-200%", "pov-gap200-300%", "pov-gap300%+"))
est_out.m <- melt(est_out)

se_out <- data.frame(cbind(c("age:18-34", "age:35-44", "age:45-54", "age:55-64", "age:65+", "whi&non-Hisp", "blac&non-Hisp", 
                             "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", ">=col", "pov-gap <50%", "pov-gap50-100%", 
                             "pov-gap100-200%", "pov-gap200-300%", "pov-gap300%+"), mar_pred_mean[sum(l_v) + 1:sum(l_v)], mar_wgt_mean[sum(l_v) + 
                                                                                                                                         1:sum(l_v), ]))
colnames(se_out) <- c("Quantity", "Str-P", "Str-W", "PS-W", "Rake-W")
se_out$Quantity <- factor(se_out$Quantity, levels = c("age:18-34", "age:35-44", "age:45-54", "age:55-64", "age:65+", 
                                                      "whi&non-Hisp", "blac&non-Hisp", "Asian", "Hisp", "other race/eth", "<high sch", "high sch", "some col", ">=col", 
                                                      "pov-gap <50%", "pov-gap50-100%", "pov-gap100-200%", "pov-gap200-300%", "pov-gap300%+"))
se_out.m <- melt(se_out)
(plot1 <- ggplot(est_out.m, aes(variable, Quantity)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(name = "Est", 
                                                                                                                            low = "white", high = "slategrey") + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 
                                                                                                                                                                                                                                                         0)) + theme_bw() + theme(axis.line = element_line(colour = "black"), axis.text = element_text(size = 12), 
                                                                                                                                                                                                                                                                                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()))
ggsave("plot/lsw_mar_est.pdf")

(plot2 <- ggplot(se_out.m, aes(variable, Quantity)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_gradient(name = "SE", 
                                                                                                                           low = "white", high = "steelblue") + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 
                                                                                                                                                                                                                                                        0)) + theme_bw() + theme(axis.line = element_line(colour = "black"), axis.text = element_text(size = 12), 
                                                                                                                                                                                                                                                                                 panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank()))

ggsave("plot/lsw_mar_se.pdf")

###-----------interaction---------------###

# 170 age>=65 & poverty gap 5 300%+
cell_index <- (1:J_sup)[cell_str[, 1] == 5 & cell_str[, 4] == 5]

# 57 2 Black Non-Hispanic & poverty gap under 50%
cell_index <- (1:J_sup)[cell_str[, 2] == 2 & cell_str[, 4] == 1]

# 222 age 35-64 1 White Non-Hispani 5 300%+ highly educated
cell_index <- (1:J_sup)[cell_str[, 2] == 1 & cell_str[, 3] == 4 & cell_str[, 4] < 3 & cell_str[, 1] > 1 & cell_str[, 
                                                                                                                   1] < 5]

id_index <- cell_id %in% cell_index
for (s in 1:dim(output$mu_cell_pred)[1]) {
  st_est_sm[s] <- sum(output$mu_cell_pred[s, J_pop %in% cell_index] * N_cell_true[J_pop %in% cell_index])/sum(N_cell_true[J_pop %in% 
                                                                                                                            cell_index])
}
est_sub_st_int <- mean(st_est_sm)
sd_sub_st_int <- sd(st_est_sm)

# model-based weights under st prior
est_st_wt <- sum(w_unit[id_index] * dat$Y[id_index])/sum(w_unit[id_index])
est_sub_st_wt_int <- est_st_wt
sd_sub_st_wt_int <- sqrt(sum(w_unit[id_index]^2 * var(dat$Y[id_index])))/sum(w_unit[id_index])

# ps weights
est_ps_wt <- sum(w_ps[id_index] * dat$Y[id_index])/sum(w_ps[id_index])
est_sub_ps_wt_int <- est_ps_wt
sd_sub_ps_wt_int <- sqrt(sum(w_ps[id_index]^2 * var(dat$Y[id_index])))/sum(w_ps[id_index])

# rake weights
est_rake_wt <- sum(w_rake[id_index] * dat$Y[id_index])/sum(w_rake[id_index])
est_sub_rake_wt_int <- est_rake_wt
sd_sub_rake_wt_int <- sqrt(sum(w_rake[id_index]^2 * var(dat$Y[id_index])))/sum(w_rake[id_index])

int_wgt_mean <- data.frame(cbind(c(est_sub_st_int, sd_sub_st_int), c(est_sub_st_wt_int, sd_sub_st_wt_int), c(est_sub_rake_wt_int, 
                                                                                                             sd_sub_rake_wt_int), c(est_sub_ps_wt_int, sd_sub_ps_wt_int)))
rownames(int_wgt_mean) <- c("Est", "SE")
colnames(int_wgt_mean) <- c("Str-P", "Str-W", "Rake-W", "PS-W")

print(xtable(int_wgt_mean, digits = 3))