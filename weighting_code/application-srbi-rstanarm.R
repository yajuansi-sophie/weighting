###---------------MRP to Construct Survey Weights--------------###
### Author: YS Latest Edit date: 07/19/2017 clear
remove(list = objects())

#----------required packages-----------------#
library(rstanarm) # requires 'structured_prior_merge' branch
library(survey)
library(ggplot2)
library(directlabels)
library(gridExtra)
library(xtable)
library(reshape2)
library(dplyr)
library(foreign)

set.seed(20150213)

source('helper_functions.R')

# prepare data
data <- read.dta("data/SRBIandAGENCYbaselinew1w2w3datawithwghts072114.dta")
SRBIdata <- 
  data %>% 
  filter(sample == "SRBI") %>%
  transmute(
    age = 
      cut(age, 
          breaks = c(0,34,44,54,64,Inf), 
          right = TRUE, 
          labels = FALSE),
    inc = dplyr::recode(
      povgap,
      '1 Under 50%' = 1,
      '2 50-100%' = 2,
      '3 100-200%' = 3,
      '4 200-300%' = 4,
      '5 300%+' = 5
    ),  
    sex = as.numeric(as.factor(r_gender)),
    sex = as.factor(sex),
    age = as.factor(age),
    eth = as.factor(race),
    inc = as.factor(as.integer(inc)),
    age = as.factor(age),
    edu = as.factor(educat),
    eld = as.factor(if_else(eldx > 1, 3, eldx + 1)),
    cld = as.factor(if_else(childx > 2, 4, childx + 1)),
    ps = as.factor(if_else(personx > 4, 4L, personx)), 
    Y = as.numeric(bestlife)
  )

SRBIdata <- within(SRBIdata, {
  Y[is.na(Y)] <- sample(Y[!is.na(Y)], sum(is.na(Y)), replace = TRUE)
})


# prepare population data (ACS)
acs_ad <- readRDS('data/acs_ad.RDS')

acs_ad %>% 
  mutate(
    cell_id =  paste0(age, eth, edu, inc)
  ) -> acs_ad

J_age <- length(unique(acs_ad$age))
J_eth <- length(unique(acs_ad$eth))
J_edu <- length(unique(acs_ad$edu))
J_inc <- length(unique(acs_ad$inc))

acs_design <- svydesign(id = ~1, weights = ~perwt, data = acs_ad)
agg_pop <- 
  svytable( ~ age + eth + edu + inc, acs_design) %>% 
  as.data.frame() %>%
  rename(N = Freq) %>%
  mutate(
    cell_id = paste0(age, eth, edu, inc)
  ) %>%
  filter(cell_id %in% acs_ad$cell_id)


###--------Four variable case: age, race, edu and inc------------###
dat <-
  SRBIdata %>%
  mutate(
    cell_id = paste0(age, eth, edu, inc)
  )

# prepare data to pass to rstanarm
dat_rstanarm <-
  dat %>%
  group_by(age, eth, edu, inc) %>%
  summarise(
    sd_cell = sd(Y),
    n = n(),
    Y = mean(Y),
    cell_id = first(cell_id)
  ) %>%
  mutate(sd_cell = if_else(is.na(sd_cell), 0, sd_cell)) %>%
  left_join(agg_pop[, c('cell_id', 'N')], by = 'cell_id')


###-----------------STAN with structural prior--------------------------###

fit <-
  stan_glmer(
    formula = 
      Y ~ 1 + (1 | age) + (1 | eth) + (1 | edu) + (1 | inc) +
      (1 | age:eth) + (1 | age:edu) + (1 | age:inc) + (1 | eth:edu) + (1 | eth:inc) + 
      (1 | age:eth:edu) + (1 | age:eth:inc),
    data = dat_rstanarm,
    iter = 1000,
    chains = 4,
    cores = 4,
    prior_covariance = 
      rstanarm::mrp_structured(
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

output <- sum_svey_model(fit, agg_pop)

###------model-based (mb) weighting------###
mb_out <-
  dat %>%
  select(cell_id, Y) %>%
  left_join(output$mean_w_new, by = 'cell_id') %>%
  mutate(w = w_unit / mean(w_unit),
         Y_w = w * Y)

output$mu_w <- mean(mb_out$Y_w)
output$w_unit <- mb_out$w

mb_design <- svydesign(id = ~ 1, data = dat, weights = output$w_unit)
svymean(~Y, mb_design)

###------PS weighting------###
w_ps_df <-
  dat %>%
  select(cell_id, Y) %>%
  left_join(agg_pop[, c('cell_id', 'N')], by = 'cell_id') %>%
  left_join(dat_rstanarm[, c('cell_id', 'n')], by = 'cell_id') %>%
  mutate(w = N / n,
         w = w / mean(w),
         Y_w = w * Y)

ps_design <- svydesign(id = ~1, data = dat, weights = w_ps_df$w)
svymean(~Y, ps_design)

###------raking estimator------###
pop_margins <- list(
  age = data.frame(age = 1:J_age, Freq = as.numeric(svytable( ~ age, acs_design))),
  eth = data.frame(eth = 1:J_eth, Freq = as.numeric(svytable( ~ eth, acs_design))),
  edu = data.frame(edu = 1:J_edu, Freq = as.numeric(svytable( ~ edu, acs_design))),
  inc = data.frame(inc = 1:J_inc, Freq = as.numeric(svytable(~ inc, acs_design)))
)
rake_output <-
  rake(
    design = svydesign(id = ~ 1, data = dat),
    sample.margins = list( ~ age, ~ eth, ~ edu, ~ inc),
    population.margins = pop_margins,
    control = list(maxit = 10, epsilon = 1, verbose = FALSE)
  )
w_rake_df <-
  data.frame(
    w = weights(rake_output)/mean(weights(rake_output)),
    Y = dat$Y,
    cell_id = dat$cell_id
  ) %>%
  mutate(Y_w = w * Y)

rake_design <- svydesign(id = ~1, data = dat, weights = w_rake_df$w)
svymean(~Y, rake_design)

# plots and tables ------------------------------------------------------

w_unit <- output$w_unit
w_rake <- w_rake_df$w
w_ps <- w_ps_df$w
n <- nrow(dat)


theme_set(bayesplot::theme_default(base_family = "sans"))

# log-weights distributions
log_weights_plot <-
  data.frame(
    wt = log(c(w_unit, w_rake, w_ps)),
    method = factor(rep(c("Str-W", "Rake-W", "PS-W"), each = n),
                    levels = c("Str-W", "Rake-W", "PS-W"))
  ) %>%
  ggplot(aes(x = wt, group = method)) +
  geom_density(aes(color = method)) +
  scale_x_continuous(name = "Distributions of log(weights) in the LSW") +
  scale_y_continuous(name = "",limits = c(0, 1.6)) +
  coord_cartesian(expand = FALSE) +
  theme(axis.line.y = element_blank())+
  bayesplot::xaxis_text(size = 20) +
  bayesplot::xaxis_title(size = 28) +
  bayesplot::yaxis_text(FALSE) +
  bayesplot::yaxis_title(FALSE)
direct.label(log_weights_plot)
ggsave("plot/weight-lsw.pdf")



# Weighted distribution of life satisfaction score in the LSW
data.frame(
  wt = c(w_unit/sum(w_unit), w_rake/sum(w_rake), w_ps/sum(w_ps), rep(1/n, n)),
  method = factor(rep(c("Str-W", "Rake-W", "PS-W", "Sample"), each = n),
                  levels = c("Str-W", "Rake-W", "PS-W", "Sample")),
  Y = dat$Y
) %>%
  ggplot(aes(x = Y, weights = wt, group = method)) +
  stat_density(
    aes(color = method),
    size = 1/3,
    geom = "line",
    position = "identity"
  ) +
  scale_color_discrete("") +
  labs(x = "Weighted distribution of life satisfaction score in the LSW", y = NULL) +
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.line.y = element_blank(),axis.ticks = element_blank(),legend.position = c(0.2,0.75),legend.text = element_text(size = 20))+
  bayesplot::xaxis_text(size = 20) +
  bayesplot::xaxis_title(size = 28) +
  bayesplot::yaxis_text(FALSE) +
  bayesplot::yaxis_title(FALSE)

ggsave("plot/weighted-lsw-density.pdf")

# over_mean_wgt
over_mean_wgt <-
  data.frame(
    "Str-P" = c(
      sd(w_unit),
      max(w_unit) / min(w_unit),
      mean(output$theta_pred),
      sd(output$theta_pred)
    ),
    "Str-W" = c(
      sd(w_unit),
      max(w_unit) / min(w_unit),
      sum(w_unit * dat$Y) / sum(w_unit),
      sqrt(sum(w_unit ^ 2 * var(dat$Y))) / n
    ),
    "Raking" = c(
      sd(w_rake),
      max(w_rake) / min(w_rake),
      sum(w_rake * dat$Y) / sum(w_rake),
      sqrt(sum(w_rake ^ 2 * var(dat$Y))) / n
    ),
    "PS" = c(
      sd(w_ps),
      max(w_ps) / min(w_ps),
      sum(w_ps * dat$Y) / sum(w_ps),
      sqrt(sum(w_ps ^ 2 * var(dat$Y))) / n
    ),
    row.names = c("SD.W", "Max/Min.W", "Est", "SE")
  )
print(xtable(over_mean_wgt, digits = 2))

### marginal means sub domain
l_v <- c(0, J_age, J_eth, J_edu, J_inc)
est_sub_st <-
  sd_sub_st <-
  est_sub_st_wt <-
  sd_sub_st_wt <-
  est_sub_st_wt2 <-
  sd_sub_st_wt2 <-
  est_sub_ps_wt <-
  sd_sub_ps_wt <-
  est_sub_ips_wt <-
  sd_sub_ips_wt <-
  est_sub_rake_wt <-
  sd_sub_rake_wt <- rep(0, sum(l_v))

cell_str <- agg_pop[, c('age','eth','edu','inc')]
for (v in 1:ncol(cell_str)) {
  for (l in 1:l_v[v + 1]) {
    sub_pop_data <-
      agg_pop %>%
      filter(cell_str[, v] == l) %>%
      select(cell_id, N) %>%
      mutate(p = N / sum(N))

    st_est_sm <- output$mu_cell_pred[, sub_pop_data$cell_id] %*% sub_pop_data$p
    est_sub_st[l + sum(l_v[1:v])] <- mean(st_est_sm)
    sd_sub_st[l + sum(l_v[1:v])] <- sd(st_est_sm)

    # model-based weights under st prior
    w_sum <- sum_weights(weight_df = mb_out, idx = sub_pop_data$cell_id, comp_stat = 0)
    est_sub_st_wt[l + sum(l_v[1:v])] <- w_sum$est_wt
    sd_sub_st_wt[l + sum(l_v[1:v])] <- w_sum$sd_wt

    # ps weights
    w_sum <- sum_weights(weight_df = w_ps_df, idx = sub_pop_data$cell_id, comp_stat = 0)
    est_sub_ps_wt[l + sum(l_v[1:v])] <- w_sum$est_wt
    sd_sub_ps_wt[l + sum(l_v[1:v])] <- w_sum$sd_wt

    # rake weights
    w_sum <- sum_weights(weight_df = w_rake_df, idx = sub_pop_data$cell_id, comp_stat = 0)
    est_sub_rake_wt[l + sum(l_v[1:v])] <- w_sum$est_wt
    sd_sub_rake_wt[l + sum(l_v[1:v])] <- w_sum$sd_wt
  }
}

marg_means <- data.frame(
  "Str.P" = c(est_sub_st, sd_sub_st),
  "Str.W" = c(est_sub_st_wt, sd_sub_st_wt),
  "PS" = c(est_sub_ps_wt, sd_sub_ps_wt),
  "Raking" = c(est_sub_rake_wt, sd_sub_rake_wt)
)

quant_nms <-
  c(
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
    "pov-gap <50%",
    "pov-gap50-100%",
    "pov-gap100-200%",
    "pov-gap200-300%",
    "pov-gap300%+"
  )

nquant <- length(quant_nms)
est_out <- data.frame(quantity = factor(quant_nms, levels = quant_nms),
                      marg_means[1:nquant, ])
est_out.m <- melt(est_out, id.vars = "quantity")

se_out <- data.frame(quantity = factor(quant_nms, levels = quant_nms),
                     marg_means[nquant + 1:nquant, ])
se_out.m <- melt(se_out, id.vars = "quantity")

theme_set(bayesplot::theme_default(base_family = "sans"))

(plot1 <-
  ggplot(est_out.m, aes(variable, quantity)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(name = "Est", low = "white", high = "slategrey") +
  labs(x = "", y = "") +coord_cartesian(expand = FALSE) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 20)
  )
)
ggsave("plot/lsw_mar_est.pdf")

(
  plot2 <-
    ggplot(se_out.m, aes(variable, quantity)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient(name = "SE", low = "white", high = "steelblue") +
    labs(x = "", y = "") +coord_cartesian(expand = FALSE) +
    theme(
      axis.line = element_line(colour = "black"),
      axis.text = element_text(size = 20)
    )
)
ggsave("plot/lsw_mar_se.pdf")

###-----------interaction---------------###

# # 154 age>=65 & poverty gap < 200%+
sub_cell_idx <-
  agg_pop %>%
  filter(as.integer(age) == 5 & as.integer(inc) < 4) %>%
  select(cell_id, N) %>%
  mutate(p = N / sum(N))

dat_sub_cell_idx <-
  dat_rstanarm %>% ungroup(age, eth, edu, inc) %>%
  filter(as.integer(age) == 5 & as.integer(inc) < 4) %>%
  select(cell_id, n)

# # 57 2 Black Non-Hispanic & poverty gap under 50%
sub_cell_idx <-
  agg_pop %>%
  filter(as.integer(eth) == 2 & as.integer(inc) == 1) %>%
  select(cell_id, N) %>%
  mutate(p = N / sum(N))

dat_sub_cell_idx <-
  dat_rstanarm %>% ungroup(age, eth, edu, inc) %>%
  filter(as.integer(eth) == 2 & as.integer(inc) == 1) %>%
  select(cell_id, n)

# 222 age 35-64 1 White Non-Hispani 5 300%+ highly educated
sub_cell_idx <-
  agg_pop %>%
  filter(as.integer(eth) == 1 &
           as.integer(edu) == 4 &
           as.integer(inc) == 5 & as.integer(age) %in% 2:4) %>%
  select(cell_id, N) %>%
  mutate(p = N / sum(N))

dat_sub_cell_idx <-
  dat_rstanarm %>% ungroup(age, eth, edu, inc) %>%
  filter(as.integer(eth) == 1 &
           as.integer(edu) == 4 &
           as.integer(inc) == 5 & as.integer(age) %in% 2:4) %>%
  select(cell_id, n)

sum(dat_sub_cell_idx$n)

st_est_sm <- output$mu_cell_pred[, sub_cell_idx$cell_id] %*% sub_cell_idx$p
est_sub_st_int <- mean(st_est_sm)
sd_sub_st_int <- sd(st_est_sm)

# model-based weights under st prior
w_sum_mb <-
  sum_weights(weight_df = mb_out,
              idx = sub_cell_idx$cell_id,
              comp_stat = 0)
est_sub_st_wt_int <- w_sum_mb$est_wt
sd_sub_st_wt_int <- w_sum_mb$sd_wt

# ps weights
w_sum_ps <-
  sum_weights(weight_df = w_ps_df,
              idx = sub_cell_idx$cell_id,
              comp_stat = 0)
est_sub_ps_wt_int <- w_sum_ps$est_wt
sd_sub_ps_wt_int <- w_sum_ps$sd_wt

# rake weights
w_sum_rake <-
  sum_weights(weight_df = w_rake_df,
              idx = sub_cell_idx$cell_id,
              comp_stat = 0)
est_sub_rake_wt_int <- w_sum_rake$est_wt
sd_sub_rake_wt_int <- w_sum_rake$sd_wt

int_wgt_mean <-
  data.frame(
    "Str.P" = c(est_sub_st_int, sd_sub_st_int),
    "Str.W" = c(est_sub_st_wt_int, sd_sub_st_wt_int),
    "Raking" = c(est_sub_rake_wt_int, sd_sub_rake_wt_int),
    "PS" = c(est_sub_ps_wt_int, sd_sub_ps_wt_int),
    row.names = c("Est", "SE")
  )
print(xtable(int_wgt_mean, digits = 2))

# weighted difference check
pop_tab <- svytable( ~ age, acs_design) / sum(svytable( ~ age, acs_design))
samp_tab_mb <- svytable( ~ age, mb_design) / n
samp_tab_ps <- svytable( ~ age, ps_design) / n
samp_tab_rake <- svytable( ~ age, rake_design) / n

pop_tab <- svytable( ~ eth, acs_design) / sum(svytable( ~ eth, acs_design))
samp_tab_mb <- svytable( ~ eth, mb_design) / n
samp_tab_ps <- svytable( ~ eth, ps_design) / n
samp_tab_rake <- svytable( ~ eth, rake_design) / n

pop_tab <- svytable( ~ edu, acs_design) / sum(svytable( ~ edu, acs_design))
samp_tab_mb <- svytable( ~ edu, mb_design) / n
samp_tab_ps <- svytable( ~ edu, ps_design) / n
samp_tab_rake <- svytable( ~ edu, rake_design) / n

pop_tab <- svytable( ~ inc, acs_design) / sum(svytable( ~ inc, acs_design))
samp_tab_mb <- svytable( ~ inc, mb_design) / n
samp_tab_ps <- svytable( ~ inc, ps_design) / n
samp_tab_rake <- svytable( ~ inc, rake_design) / n

pop_tab <- svytable( ~ eth + age, acs_design) / sum(svytable( ~ eth + age, acs_design))
samp_tab_mb <- svytable( ~ eth + age, mb_design) / n
samp_tab_ps <- svytable( ~ eth + age, ps_design) / n
samp_tab_rake <- svytable( ~ eth + age, rake_design) / n

pop_tab <- svytable( ~ edu + age, acs_design) / sum(svytable( ~ edu + age, acs_design))
samp_tab_mb <- svytable( ~ edu + age, mb_design) / n
samp_tab_ps <- svytable( ~ edu + age, ps_design) / n
samp_tab_rake <- svytable( ~ edu + age, rake_design) / n

pop_tab <- svytable( ~ inc + age, acs_design) / sum(svytable( ~ inc + age, acs_design))
samp_tab_mb <- svytable( ~ inc + age, mb_design) / n
samp_tab_ps <- svytable( ~ inc + age, ps_design) / n
samp_tab_rake <- svytable( ~ inc + age, rake_design) / n

pop_tab <- svytable( ~ inc + eth, acs_design) / sum(svytable( ~ inc + eth, acs_design))
samp_tab_mb <- svytable( ~ inc + eth, mb_design) / n
samp_tab_ps <- svytable( ~ inc + eth, ps_design) / n
samp_tab_rake <- svytable( ~ inc + eth, rake_design) / n

pop_tab <- svytable( ~ edu + eth, acs_design) / sum(svytable( ~ edu + eth, acs_design))
samp_tab_mb <- svytable( ~ edu + eth, mb_design) / n
samp_tab_ps <- svytable( ~ edu + eth, ps_design) / n
samp_tab_rake <- svytable( ~ edu + eth, rake_design) / n

pop_tab <- svytable( ~ edu + inc, acs_design) / sum(svytable( ~ edu + inc, acs_design))
samp_tab_mb <- svytable( ~ edu + inc, mb_design) / n
samp_tab_ps <- svytable( ~ edu + inc, ps_design) / n
samp_tab_rake <- svytable( ~ edu + inc, rake_design) / n

pop_tab <- svytable( ~ edu + age + eth, acs_design) / sum(svytable( ~ edu + age + eth, acs_design))
samp_tab_mb <- svytable( ~ edu + age + eth, mb_design) / n
samp_tab_ps <- svytable( ~ edu + age + eth, ps_design) / n
samp_tab_rake <- svytable( ~ edu + age + eth, rake_design) / n

pop_tab <- svytable( ~ inc + age + eth, acs_design) / sum(svytable( ~ inc + age + eth, acs_design))
samp_tab_mb <- svytable( ~ inc + age + eth, mb_design) / n
samp_tab_ps <- svytable( ~ inc + age + eth, ps_design) / n
samp_tab_rake <- svytable( ~ inc + age + eth, rake_design) / n

pop_tab <- svytable( ~ inc + age + edu, acs_design) / sum(svytable( ~ inc + age + edu, acs_design))
samp_tab_mb <- svytable( ~ inc + age + edu, mb_design) / n
samp_tab_ps <- svytable( ~ inc + age + edu, ps_design) / n
samp_tab_rake <- svytable( ~ inc + age + edu, rake_design) / n

pop_tab <- svytable( ~ inc + eth + edu, acs_design) / sum(svytable( ~ inc + eth + edu, acs_design))
samp_tab_mb <- svytable( ~ inc + eth + edu, mb_design) / n
samp_tab_ps <- svytable( ~ inc + eth + edu, ps_design) / n
samp_tab_rake <- svytable( ~ inc + eth + edu, rake_design) / n

pop_tab <- svytable( ~ inc + age + edu+ eth, acs_design) / sum(svytable( ~ inc + age + edu+ eth, acs_design))
samp_tab_mb <- svytable( ~ inc + age + edu+ eth, mb_design) / n
samp_tab_ps <- svytable( ~ inc + age + edu+ eth, ps_design) / n
samp_tab_rake <- svytable( ~ inc + age + edu+ eth, rake_design) / n


print(
  xtable(
    cbind(
      "Model-based" = sqrt(sum((pop_tab - samp_tab_mb) ^ 2)),
      "PS" = sqrt(sum((pop_tab - samp_tab_ps) ^ 2)),
      "Raking" = sqrt(sum((pop_tab - samp_tab_rake) ^ 2))
    ),
    digits = 2)
)
