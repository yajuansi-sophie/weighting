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
# @param cell_table A data frame with columns "N" (population cell counts) 
#   and "n" (sample cell counts).
# @return A matrix with number of rows equal to the number of posterior draws 
#   and number of columns equal to the number of cells, i.e. nrow(cell_table).
#
shrinkage_factor <- function(object, cell_table) {
  var_draws <- pri_var(object)
  inv_sigma_theta_sq <- 1 / var_draws$sigma_theta_sq
  sigma_y_sq <- var_draws$sigma_y_sq
  
  ns <- cell_table[["n"]]  # sample cell counts
  J <- nrow(cell_table)
  
  ps_w <- matrix(NA, nrow = length(sigma_y_sq), ncol = J)
  for (j in 1:J) {
    inv_obs_var_cell = ns[j] / sigma_y_sq 
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