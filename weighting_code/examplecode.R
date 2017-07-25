###-----example code-----###
model_based_cell_weights <- function(object, cell_table) {
  stopifnot(
    is.data.frame(cell_table),
    colnames(cell_table) == c("N", "n")
  )
  draws <- as.matrix(object)
  Sigma <- draws[, grep("^Sigma\\[", colnames(draws)), drop = FALSE]
  sigma_theta_sq <- rowSums(Sigma)
  sigma_y_sq <- draws[, "sigma"]^2
  Ns <- cell_table[["N"]]  # population cell counts
  ns <- cell_table[["n"]]  # sample cell counts
  J <- nrow(cell_table)
  N <- sum(Ns)
  n <- sum(ns)
  # implementing equation 7 in the paper (although i did some algebra first to 
  # simplify the expression a bit)
  Nsy2 <- N * sigma_y_sq
  ww <- matrix(NA, nrow = nrow(draws), ncol = J)
  for (j in 1:J) {
    ww[, j] <- 
      (Nsy2 + n * Ns[j] * sigma_theta_sq) / (Nsy2 + N * ns[j] * sigma_theta_sq)
  }
  return(ww)
}
# prepare population data: acs_ad has age, eth, edu and inc
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
    cell_id = paste0(age, eth, edu, inc) 
  ) %>%
  filter(cell_id %in% acs_ad$cell_id)
# prepare data to pass to rstanarm
# SURVEYdata has 4 weighting variables: age, eth, edu and inc; and outcome Y
dat_rstanarm <-
  SURVEYdata %>%
  mutate(
    cell_id = paste0(age, eth, edu, inc)
  )%>% 
  group_by(age, eth, edu, inc) %>%
  summarise(
    sd_cell = sd(Y),
    n = n(),
    Y = mean(Y),
    cell_id = first(cell_id)
  ) %>%
  mutate(sd_cell = if_else(is.na(sd_cell), 0, sd_cell)) %>%
  left_join(agg_pop[, c("cell_id", "N")], by = "cell_id")
# Stan fitting under structured prior in rstanarm
fit <-
  stan_glmer(
    formula = 
      Y ~ 1 + (1 | age) + (1 | eth) + (1 | edu) + (1 | inc) +
      (1 | age:eth) + (1 | age:edu) + (1 | age:inc) +
      (1 | eth:edu) + (1 | eth:inc) + 
      (1 | age:eth:edu) + (1 | age:eth:inc),
    data = dat_rstanarm,  iter = 1000, chains = 4, cores = 4,
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
# model-based weighting
cell_table <- fit$data[,c("N","n")]
weights <- model_based_cell_weights(fit, cell_table)
weights <- data.frame(w_unit = colMeans(weights),
                      cell_id = fit$data[["cell_id"]],
                      Y = fit$data[["Y"]],
                      n = fit$data[["n"]]) %>%
  mutate(
    w = w_unit / sum(n / sum(n) * w_unit), # model-based weights
    Y_w = Y * w
  ) 
with(weights, sum(n * Y_w / sum(n)))# mean estimate
