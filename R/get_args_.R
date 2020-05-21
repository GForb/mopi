get_args_weighted <- function(weights, new_x, processed_lavaan) {

  b <- matrix(processed_lavaan$beta$est, ncol = 1)
  sigma_beta <- processed_lavaan$sigma_beta
  x_long <- create_x_matrix(new_x, processed_lavaan$beta)
  adjusted_weights <- adjust_weights(weights, processed_lavaan$rescale_df, processed_lavaan$beta)
  sigma_e_weighted <- weight_sigma_e(adjusted_weights$w, processed_lavaan$sigma_e)
  mean_adjustment <- weight_mean_adjustment(adjusted_weights$w, processed_lavaan$rescale_df)

  return(list(beta = b,
              sigma_e = sigma_e_weighted,
              sigma_beta = sigma_beta,
              weights = adjusted_weights$w_long,
              mean_adjustment = mean_adjustment,
              x = x_long))
}

get_args_single_outcome <- function(outcome, new_x, processed_lavaan) {
  beta <- processed_lavaan$beta[processed_lavaan$beta$lhs == outcome,]
  b = matrix(beta$est, ncol = 1)
  sigma_beta <- processed_lavaan$sigma_beta[beta$eqname, beta$eqname, drop = FALSE]
  rescale_df <- processed_lavaan$rescale_df[processed_lavaan$rescale_df$outcome == outcome, ]
  rescale_factor <- rescale_df$scale
  rescale_mean <- rescale_df$mean*rescale_factor
  sigma_e <- processed_lavaan$sigma_e[outcome, outcome]
  adj_sigma_e <- as.vector(sigma_e)*rescale_factor^2

  x <- create_x_matrix(new_x, beta)

  return(list(x = x,
              beta = b,
              sigma_beta = sigma_beta,
              sigma_e = adj_sigma_e,
              weights = rescale_factor,
              mean_adjustments = rescale_mean))
}



create_x_matrix <- function(new_x, beta) {
  new_x <- c(new_x, intercept = 1)
  x_long <- matrix(new_x[beta$rhs], ncol = 1)
  return(x_long)
}


adjust_weights <- function(weights, rescale_df, beta) {
  rescale_factors  <- rescale_df$scale
  names(rescale_factors) <- rescale_df$outcome
  adj_weights <- weights*rescale_factors[names(weights)]
  w_long <- matrix(adj_weights[beta$lhs], ncol = 1)
  return(list(w_long = w_long, w = adj_weights))
}

weight_sigma_e <- function(adj_weights, sigma_e) {
  w <- matrix(adj_weights[rownames(sigma_e)], ncol = 1)
  sigma_e_weighted  <-  t(w) %*% sigma_e %*% w
  return(as.vector(sigma_e_weighted))
}


weight_mean_adjustment <- function(adj_weights, rescale_df){
  rescale_means <- matrix(rescale_df$mean, nrow = 1)
  adj_weights <- matrix(adj_weights[rescale_df$outcome], ncol = 1)
  weighted_mean_adjustment <-  rescale_means %*% adj_weights
  return(as.vector(weighted_mean_adjustment))
}
