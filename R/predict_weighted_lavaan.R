predict_weighted_lavaan <- function(new_x, weights, lavaan_model, predict_outcomes = TRUE) {
  esstimates <- process_lavaan_estimates(lavaan_model)
  prediction_matricies <- get_args_weighted(new_x = new_x,
                                            weights = weights,
                                            esstimates)

  predictions <- do.call(prediction_intervals, prediction_matricies)
  predictions$outcome <- "weighted"
  if(predict_outcomes){
    outcome_predictions <- predict_outcomes_lavaan(new_x = new_x, processed_lavaan = esstimates)
    predictions <- rbind(outcome_predictions, predictions)
  }
  return(predictions)
}




predict_outcomes_lavaan <- function(new_x, processed_lavaan) {
  outcomes <- unique(processed_lavaan$beta$lhs)

  results <- purrr::map_dfr(outcomes,
                            predict_single_outcome,
                            new_x = new_x,
                            processed_lavaan = processed_lavaan)

  return(results)
}

predict_single_outcome <- function(outcome, new_x, processed_lavaan) {
  args <- get_args_single_outcome(outcome, new_x, processed_lavaan)
  results <- do.call(prediction_intervals, args)
  results$outcome <- outcome
  return(results)
}

get_single_outcome_args <- function(outcome, new_x, processed_lavaan) {
  beta <- matrix(processed_lavaan$beta[processed_lavaan$beta$lhs == outcome], ncol = 1)
  sigma_beta <- processed_lavaan$sigma_beta[beta$eqname, beta$eqname]
  rescale_df <- processed_lavaan$rescale_df[processed_lavaan$rescale_df$outcome == outcome, ]
  rescale_factor <- rescale_df$scale
  rescale_mean <- rescale_df$mean
  sigma_e <- processed_lavaan$sigma_e[outcome, outcome]
  adj_sigma_e <- as.vector(sigma_e)*rescale_factor

  new_x <- matrix(new_x[beta$rhs], ncol = 1)

  return(list(x = new_x,
              beta = beta,
              sigma_beta = sigma_beta,
              sigma_e = adj_sigma_e,
              weights = rescale_factor,
              mean_adjustments = rescale_mean))
}

prediction_intervals <- function(x, beta, sigma_e, sigma_beta, weights = 1, mean_adjustments = 0 ) {
  weighted_x = weights*x
  predicted_mean <-   (t(weighted_x) %*% beta) - mean_adjustments
  var_mean_pred <- t(weighted_x) %*% sigma_beta %*% weighted_x
  var_pred = var_mean_pred + sigma_e

  ci_multiplier <- stats::qnorm(0.975)

  predictions <- data.frame(.fitted = predicted_mean,
                            .se.fit = sqrt(var_mean_pred),
                            .se.pred = sqrt(var_pred))
  predictions$.conf.low <-  predictions$.fitted - (ci_multiplier*predictions$.se.fit)
  predictions$.conf.high <-  predictions$.fitted + (ci_multiplier*predictions$.se.fit)
  predictions$.pred.low  <-  predictions$.fitted - (ci_multiplier*predictions$.se.pred)
  predictions$.pred.high <-  predictions$.fitted + (ci_multiplier*predictions$.se.pred)

  return(predictions)
}



