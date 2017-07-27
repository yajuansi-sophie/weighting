# Compute observational variance and trace of the covariance matrix
# 
# @param object stanreg object (fitted rstanarm model)
# @return two-element list
#
pri_var <- function(object) {
  draws <- as.matrix(object)
  Sigma <- draws[, grep("^Sigma\\[", colnames(draws)), drop = FALSE]
  sigma_theta_sq <- rowSums(Sigma)
  sigma_y_sq <- draws[, "sigma"]^2
  return(list(sigma_theta_sq = sigma_theta_sq, sigma_y_sq = sigma_y_sq))
}

# Compute shrinkage factor for each post-stratification cell
# 
# @param object stanreg object (fitted rstanarm model)
# @param ns A vector of "n" (sample cell counts).
# @return A matrix with number of rows equal to the number of posterior draws 
#   and number of columns equal to the number of cells.
#
shrinkage_factor <- function(object, ns) {
  var_draws <- pri_var(object)
  inv_sigma_theta_sq <- 1 / var_draws$sigma_theta_sq
  sigma_y_sq <- var_draws$sigma_y_sq
  
  J <- length(ns)
  
  ps_w <- matrix(NA, nrow = length(sigma_y_sq), ncol = J)
  for (j in 1:J) {
    inv_obs_var_cell <- ns[j] / sigma_y_sq 
    ps_w[, j] <- 
      inv_obs_var_cell / (inv_obs_var_cell + inv_sigma_theta_sq)
  }
  return(ps_w)
}

# Compute model-based weights for each post-stratification cell
# 
# @param object stanreg object (fitted rstanarm model)
# @param cell_table A data frame with columns "N" (population cell counts) 
#   and "n" (sample cell counts).
# @return A matrix with number of rows equal to the number of posterior draws 
#   and number of columns equal to the number of cells, i.e. nrow(cell_table).
#
model_based_cell_weights <- function(object, cell_table) {
  stopifnot(
    is.data.frame(cell_table),
    colnames(cell_table) == c("N", "n")
  )
  var_draws <- pri_var(object)
  sigma_theta_sq <- var_draws$sigma_theta_sq
  sigma_y_sq <- var_draws$sigma_y_sq
  
  Ns <- cell_table[["N"]]  # population cell counts
  ns <- cell_table[["n"]]  # sample cell counts
  
  J <- nrow(cell_table)
  N <- sum(Ns)
  n <- sum(ns)
  
  # implementing equation 7 in the paper (although i did some algebra first to 
  # simplify the expression a bit)
  Nsy2 <- N * sigma_y_sq
  ww <- matrix(NA, nrow = length(sigma_y_sq), ncol = J)
  for (j in 1:J) {
    ww[, j] <- 
      (Nsy2 + n * Ns[j] * sigma_theta_sq) / (Nsy2 + N * ns[j] * sigma_theta_sq)
  }
  return(ww)
}

#' @param object rstanarm fit
#' @param agg_pop poststrat frame
sum_svey_model <- function(object, agg_pop) {
  model_data <- object$data
  cell_table <- model_data[, c('N', 'n')]
  ret_list <-
    list(
      mu_cell = rstanarm::posterior_linpred(object, newdata = model_data),
      mu_cell_pred = rstanarm::posterior_linpred(object, newdata = agg_pop),
      w_new = model_based_cell_weights(object, cell_table)
    )
  colnames(ret_list$mu_cell_pred) <- agg_pop$cell_id
  colnames(ret_list$mu_cell) <- model_data$cell_id
  
  ret_list$theta_sample <-
    ret_list$mu_cell %*% (cell_table$N / sum(cell_table$N))
  ret_list$theta_pred <-
    ret_list$mu_cell_pred %*% (agg_pop$N / sum(agg_pop$N))
  ret_list$mean_w_new <-
    data.frame(w_unit = colMeans(ret_list$w_new),
               cell_id = model_data$cell_id)
  
  return(ret_list)
}

#' @param object rstanarm fit
#' @param agg_pop poststrat frame
sum_weights <- function(weight_df, idx, comp_stat) {
  sub_weight_df <- weight_df %>% filter(cell_id %in% idx)
  Y_sub <- sub_weight_df$Y
  Y_w_sub <- sub_weight_df$Y_w
  w_sub <- sub_weight_df$w
  est_wt <- sum(Y_w_sub) / sum(w_sub)
  bias <- est_wt - comp_stat
  sd_wt <- sqrt(sum(w_sub ^ 2 * var(Y_sub))) / sum(w_sub)
  cr_wt <- as.numeric(est_wt - 1.96 * sd_wt <= comp_stat & comp_stat <= est_wt + 1.96 * sd_wt)
  return(list(
    est_wt = est_wt,
    bias = bias,
    sd_wt = sd_wt,
    cr_wt = cr_wt
  ))
}
