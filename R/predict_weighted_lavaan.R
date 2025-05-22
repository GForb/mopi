#' predict_weighted_lavaan
#'
#' @param new_x vector of covariates to predict. names of x must correspond to names of x columns that were used to fit the model.
#' @param weights weights to apply to each outcome. names of w must correspond to names of outcomes in the model.
#' @param lavaan_model fitted lavaan mdoel/
#' @param predict_outcomes TRUE/FALSE: Indicates whether predictions of individual outcomes are included or only weighted prediction/
#' @param report_rescale_factors TRUE/FALSE: Indicates whether the scaling multipliers used for each outcome are inlcuded in the output.
#' @return A data.frame of results
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
#'
predict_weighted_lavaan <- function(new_x, weights, lavaan_model, predict_outcomes = TRUE, report_rescale_factors = FALSE) {
  estimates <- process_lavaan_estimates(lavaan_model)
  prediction_matricies <- get_args_weighted(new_x = new_x,
                                            weights = weights,
                                            estimates)

  predictions <- do.call(prediction_intervals, prediction_matricies)
  predictions$outcome <- "weighted"
  if(predict_outcomes){
    outcome_predictions <- predict_outcomes_lavaan(new_x = new_x, processed_lavaan = estimates)
    predictions <- rbind(outcome_predictions, predictions)
  }
  if(report_rescale_factors){
    predictions <- dplyr::left_join(predictions, estimates$rescale_df, by = "outcome")
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



