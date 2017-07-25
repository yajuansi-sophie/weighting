###-----example code-----###

# prepare data to pass to rstanarm
# SURVEYdata has four weighting variables: age, eth, edu and inc; and one outcome variable Y
dat_rstanarm <-
  SURVEYdata %>%
  mutate(
    cell_id = paste0(age, eth, edu, inc),
    j = (as.integer(inc) - 1) * J_edu * J_eth * J_age + 
      (as.integer(edu) - 1) * J_eth * J_age + 
      (as.integer(eth) - 1) * J_age + 
      as.integer(age)
  )%>% 
  arrange(j)%>%
  group_by(age, eth, edu, inc) %>%
  summarise(
    sd_cell = sd(Y),
    n = n(),
    Y = mean(Y),
    cell_id = first(cell_id),
    j = first(j)
  ) %>%
  mutate(sd_cell = if_else(is.na(sd_cell), 0, sd_cell)) %>%
  left_join(agg_pop[, c('cell_id', 'N')], by = 'cell_id') %>%
  arrange(j)
# prepare population data (ACS)
# acs_ad has age, eth, edu and inc
acs_ad %>% 
  mutate(
    cell_id =  paste0(age, eth, edu, inc)
  ) -> acs_ad
acs_design <- svydesign(id = ~1, weights = ~perwt, data = acs_ad)
agg_pop <- 
  svytable( ~ age + eth + edu + inc, acs_design) %>% 
  as.data.frame() %>%
  rename(N = Freq) %>%
  mutate(
    cell_id = paste0(age, eth, edu, inc), 
    j = (as.integer(inc) - 1) * J_edu * J_eth * J_age + 
      (as.integer(edu) - 1) * J_eth * J_age + 
      (as.integer(eth) - 1) * J_age + 
      as.integer(age)
  ) %>%
  filter(cell_id %in% acs_ad$cell_id) %>%
  arrange(j)
# Stan fitting under structured prior
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
colnames(as.matrix(fit)) #variables in the output
output <- sum_svey_model(fit, agg_pop)
# model-based weighting
mb_out <-
  dat %>%
  select(cell_id, Y) %>%
  left_join(output$mean_w_new, by = 'cell_id') %>%
  mutate(w = w_unit / mean(w_unit),
         Y_w = w * Y)
mean(mb_out$Y_w) # mean estimate
mb_out$w # model-based weights
