predict_weighted_lavaan <- function(new_x, weights, processed_lavaan, predict_outcomes = TRUE) {
  sigma_e <- processed_lavaan$sigma_e
  sigma_beta <- processed_lavaan$sigma_beta
  beta <- processed_lavaan$beta

  x <- c(new_x, "intercept" = 1)
  x_long <- matrix(x[beta$rhs], ncol = 1)
  w_long <- matrix(weights[beta$lhs], ncol = 1)
  wx <- x_long*w_long
  b <- matrix(beta$est, ncol = 1)
  w <- matrix(weights[rownames(sigma_e)], ncol = 1)
  sigma_e_weighted  <-  t(w) %*% sigma_e %*% w

  predictions <- prediction_intervals(wx, b, sigma_e_weighted, sigma_beta)
  predictions$outcome <- "weighted"
  if(predict_outcomes){
    outcome_predictions <- predict_outcomes_lavaan(new_x = x, processed_lavaan = processed_lavaan)
    predictions <- rbind(outcome_predictions, predictions)
  }
  return(predictions)
}



predict_outcomes_lavaan <- function(new_x, processed_lavaan) {
  outcomes <- unique(processed_lavaan$beta$lhs)
  results_list <- lapply(X = outcomes,
                    FUN = predict_single_outcome,
                    new_x = new_x,
                    processed_lavaan = processed_lavaan)
  results <- do.call(rbind.data.frame, results_list)
  return(results)
}

predict_single_outcome <- function(outcome_name, new_x, processed_lavaan) {
  beta_df <- processed_lavaan$beta[processed_lavaan$beta$lhs == outcome_name,]
  x <- matrix(new_x[beta_df$rhs], ncol = 1)

  sigma_e <- processed_lavaan$sigma_e[outcome_name, outcome_name]
  beta <- matrix(beta_df$est, ncol = 1)
  sigma_beta <- processed_lavaan$sigma_beta[beta_df$eqname, beta_df$eqname]

  results <- prediction_intervals(x, beta, sigma_e, sigma_beta)
  results$outcome <- outcome_name
  return(results)
}

prediction_intervals <- function(x, beta, sigma_e, sigma_beta) {
  mean_pred <-   t(x) %*% beta
  var_mean_pred <- t(x) %*% sigma_beta %*% x
  var_pred = var_mean_pred + sigma_e

  ci_multiplier <- stats::qnorm(0.975)

  predictions <- data.frame(.fitted = mean_pred,
                            .se.fit = sqrt(var_mean_pred),
                            .se.pred = sqrt(var_pred))
  predictions$.conf.low <-  predictions$.fitted - (ci_multiplier*predictions$.se.fit)
  predictions$.conf.high <-  predictions$.fitted + (ci_multiplier*predictions$.se.fit)
  predictions$.pred.low  <-  predictions$.fitted - (ci_multiplier*predictions$.se.pred)
  predictions$.pred.high <-  predictions$.fitted + (ci_multiplier*predictions$.se.pred)

  return(predictions)
}



