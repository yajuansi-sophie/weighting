###---------------MRP to Construct Survey Weights--------------###
### Author: YS Latest Edit date: 06/07/2017 clear
remove(list = objects())

#----------required packages-----------------#
require(foreign)
require(rstanarm)
require(survey)
require(ggplot2)
require(gridExtra)
library(xtable)
library(directlabels)
library(reshape2)
library(dplyr)

set.seed(20150213)

source('weighting_code/cell_weights.R')
sum_svey_model <- function(object, agg_p) {
  model_data <- object$data
  cell_table <- model_data[,c('N','n')]
  ret_list <- list(mu_cell = rstanarm::posterior_linpred(object, newdata = model_data),
                   mu_cell_pred = rstanarm::posterior_linpred(object, newdata = agg_p),
                   w_new = model_based_cell_weights(object, cell_table))
  colnames(ret_list$mu_cell_pred) <- agg_p$cell_id
  colnames(ret_list$mu_cell) <- model_data$cell_id
  ret_list$theta_sample <- ret_list$mu_cell %*% (cell_table$N / sum(cell_table$N))
  ret_list$theta_pred <- ret_list$mu_cell_pred %*% (agg_p$N / sum(agg_p$N))
  ret_list$mean_w_new <- data.frame(w_unit = colMeans(ret_list$w_new), 
                                    cell_id = model_data$cell_id)
  return(ret_list)
}

#----------data simulation-----------------#
data <- read.dta("weighting_code/data/SRBIandAGENCYbaselinew1w2w3datawithwghts072114.dta")
SRBIdata <- data[data$sample == "SRBI", ]

Y_sys <- as.numeric(SRBIdata[, names(SRBIdata) == "bestlife"])
Y_sys[is.na(Y_sys)] <- sample(Y_sys[!is.na(Y_sys)], sum(is.na(Y_sys)), replace = T)
# hist(Y_sys) ACS data
acs_pop <- read.dta("weighting_code/data/acs_nyc_2011_wpov1.dta", convert.factors = FALSE)

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
  ) %>% select(-age_gp, -opmres_gp) %>%
  mutate(
    age = as.factor(age_dc),
    eth = as.factor(race_dc),
    edu = as.factor(educat),
    sex = as.factor(sex),
    inc = as.factor(opmres_x),
    eld = as.factor(eldx_ca),
    cld = as.factor(childx_ca),
    ps = as.factor(personx_ca),
    cell_id = paste0(age, eth, edu, inc)
  )

# what about filtering under 18?
SRBIdata <- SRBIdata %>% #filter(age >= 18) %>%
  mutate(
    age_gp = cut(x = age,
                  breaks = c(0,34,44,54,64,Inf),
                  right = TRUE),
    age_dc = as.integer(age_gp),
    race_dc = race,
    sex = as.numeric(as.factor(r_gender)),
    opmres_gp = dplyr::recode(povgap, 
                              '1 Under 50%'=1, 
                              '2 50-100%'=2, 
                              '3 100-200%'=3, 
                              '4 200-300%'=4, 
                              '5 300%+'=5), 
    opmres_x = as.integer(opmres_gp),
    eldx_ca = if_else(eldx > 1, 3, eldx + 1),
    childx_ca = if_else(childx > 2, 4, childx + 1),
    personx_ca = if_else(personx > 4,4L,personx)
  ) %>% select(-age_gp, -opmres_gp) %>%
  mutate(
    age = as.factor(age_dc),
    eth = as.factor(race_dc),
    edu = as.factor(educat),
    sex = as.factor(sex),
    inc = as.factor(opmres_x),
    eld = as.factor(eldx_ca),
    cld = as.factor(childx_ca),
    ps = as.factor(personx_ca)
  )

J_age <- length(unique(acs_ad$age))
J_eth <- length(unique(acs_ad$eth))
J_edu <- length(unique(acs_ad$edu))
J_sex <- length(unique(acs_ad$sex))
J_inc <- length(unique(acs_ad$inc))
J_eld <- length(unique(acs_ad$eld))
J_cld <- length(unique(acs_ad$cld))
J_ps <- length(unique(acs_ad$ps))

acs_ds_ad <- svydesign(id = ~1, weights = ~perwt, data = acs_ad)
acs_N <- as.numeric(svytable(~age_dc + race_dc + educat + opmres_x, acs_ds_ad))
###--------Four variable case: age, race, edu and inc------------###

agg_pop <- acs_ad %>%
  group_by(age, eth, edu, inc) %>%
  summarise(N = n(),
            cell_id = first(cell_id)) %>% 
  mutate(
    j = (as.integer(inc) - 1) * J_edu * J_eth * J_age + (as.integer(edu) - 1) * J_eth * J_age + (as.integer(eth) - 1) * J_age + as.integer(age)
    ) %>% ungroup()

dat <- data.frame(Y = Y_sys, age = SRBIdata$age, eth = SRBIdata$eth, edu = SRBIdata$edu, sex = SRBIdata$sex, 
                  inc = SRBIdata$inc, eld = SRBIdata$eld, cld = SRBIdata$cld, ps = SRBIdata$ps) %>%
  mutate(cell_id = paste0(age, eth, edu, inc),
         j = (as.integer(inc) - 1) * J_edu * J_eth * J_age + (as.integer(edu) - 1) * J_eth * J_age + (as.integer(eth) - 1) * J_age + as.integer(age))

dat_rstanarm <- dat %>% 
  group_by(age, eth, edu, inc) %>%
  summarise(sd_cell = sd(Y),
            n = n(),
            Y = mean(Y),
            cell_id = first(cell_id),
            j = first(j)) %>%
  mutate(
    sd_cell = if_else(is.na(sd_cell), 0, sd_cell)
  ) %>%
  ## this line needs to be changed to match the output from svytable
  left_join(agg_pop[,c('cell_id','N')], by = 'cell_id')


###-----------------STAN with structural prior--------------------------###

ff <- as.formula(Y ~ 1 + (1 | age) + (1 | eth) + (1 | edu) + (1 | inc) +
                   (1 | age:eth) + (1 | age:edu) + (1 | age:inc) + (1 | eth:edu) +
                   (1 | eth:inc) + (1 | age:eth:edu) + (1 | age:eth:inc))
S <- stan_glmer(formula = ff, data = dat_rstanarm, iter = 8000, chains = 4, cores = 4, 
                          prior_covariance = rstanarm::mrp_structured(cell_size = dat_rstanarm$n, cell_sd = dat_rstanarm$sd_cell), 
                          seed = 123, prior_aux = cauchy(0,5), prior_intercept = normal(0, 100, autoscale = FALSE))

# tt <- rstan::extract(S$stanfit)

output_st <- sum_svey_model(S, agg_pop)

comp_fit_df <- arrange(dat_rstanarm, j)
w_new_dist <- model_based_cell_weights(S, comp_fit_df)
w_new <- colMeans(w_new_dist)


comp_pop_df <- arrange(agg_pop, j)
comp_fit_s <- posterior_linpred(S, newdata = pred_df)
comp_pred_s <- posterior_linpred(S, newdata = comp_pop_df)

# head(colMeans(comp_df))
# head(colMeans(output$mu_cell))
# head(abs(colMeans(output$mu_cell)-colMeans(comp_df)))
# tail(sort(abs(colMeans(output$mu_cell)-colMeans(comp_df))))
# hist(comp_pred_df[,tail(order(abs(colMeans(output$mu_cell_pred)-colMeans(comp_pred_df))),1)])
# hist(output$mu_cell_pred[,tail(order(abs(colMeans(output$mu_cell_pred)-colMeans(comp_pred_df))),1)])
# agg_pop_df[tail(order(abs(colMeans(output$mu_cell_pred)-colMeans(comp_pred_df))),10),]

### model-based weights
dat %>%
  left_join(
    output_st$mean_w_new, by = 'cell_id'
) %>% mutate(
  w = w_unit / mean(w_unit),
  Y_w = w * Y
) %>% select(cell_id, w, Y_w, Y) -> st_out
output_st$mu_w <- mean(st_out$Y_w)
output_st$w_unit <- st_out$w

st.dat.design <- svydesign(id = ~1, data = dat, weights = output_st$w_unit)
svymean(~Y, st.dat.design)

###------PS weighting------###
w_ps_df <- dat[,c('cell_id','Y')] %>%
  left_join(agg_pop[,c('cell_id','N')], by = 'cell_id') %>%
  left_join(dat_rstanarm[,c('cell_id','n')], by = 'cell_id') %>%
  mutate(w = N / n,
         w = w / mean(w),
         Y_w = w * Y)

ps.dat.design <- svydesign(id = ~1, data = dat, weights = w_ps_df$w)

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

w_rake_df <- data.frame(w = weights(dat_rake)/mean(weights(dat_rake)),
                        Y = dat$Y,
                        cell_id = dat$cell_id) %>%
  mutate(
    Y_w = w * Y
  )

rake.dat.design <- svydesign(id = ~1, data = dat, weights = w_rake_df$w)

# poorhealth health anyhard hardscl
st.dat.design <- svydesign(id = ~1, data = SRBIdata, weights = output_st$w_unit)
ps.dat.design <- svydesign(id = ~1, data = SRBIdata, weights = w_ps_df$w)
rake.dat.design <- svydesign(id = ~1, data = SRBIdata, weights = w_rake_df$w)

p.d <- svytable(~age_dc, acs_ds_ad)/sum(svytable(~age_dc, acs_ds_ad))
s.d <- svytable(~age, st.dat.design)/nrow(dat)

s.d.ps <- svytable(~age, ps.dat.design)/nrow(dat)

s.d.rake <- svytable(~age, rake.dat.design)/nrow(dat)

p.d <- svytable(~educat + age_dc, acs_ds_ad)/sum(svytable(~educat + age_dc, acs_ds_ad))
s.d <- svytable(~edu + age, st.dat.design)/nrow(dat)
s.d.ps <- svytable(~edu + age, ps.dat.design)/nrow(dat)
s.d.rake <- svytable(~edu + age, rake.dat.design)/nrow(dat)

print(xtable(data.frame(cbind(sqrt(sum((p.d - s.d)^2)), sqrt(sum((p.d - s.d.ps)^2)), sqrt(sum((p.d - s.d.rake)^2)))), 
             digits = 3))

weights <- data.frame(wt = log(c(output_st$w_unit, w_rake_df$w, w_ps_df$w)), 
                      Method = c(rep("Str-W", nrow(dat)), rep("Rake-W", nrow(dat)), rep("PS-W", nrow(dat)))) %>%
  mutate(
    Method = factor(Method, levels = c("Str-W", "Rake-W", "PS-W"))
 )

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