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
  predictions$rescaled <- !(predicted_mean == t(x)%*% beta)
  return(predictions)
}



