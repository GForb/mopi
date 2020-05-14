# predict_weighted <- function(data, weights, model_parameters){
#   predictions <- apply(X, MARGIN = 1, single_weighted_prediction, weights, model)
#   process(predictions)
# }

single_weighted_predction <- function(x, weights, model) {
  n_outcomes <- model$n_outcomes
  x <- get_x_matrix(x = x, covariates = model$covariates)
  w <- matrix(weights, ncol = 1)

  #Predictions
  predictions <- model$beta %*% x
  weighted_prediction <- t(w) %*% predictions

  #Standard errors
  sigma_y_hat = matrix(nrow =n_outcomes, ncol = n_outcomes)
  for(i in 1:n_outcomes) {
    for(j in 1:n_outcomes){
      sigma_y_hat[i,j] <-  t(x) %*% model$sigma_beta[,,i,j] %*% x
    }
  }
  var_y <- diag(sigma_y_hat)
  se_y <- sqrt(var_y)
  var_pred_y <- var_y + diag(model$sigma_e)
  se_pred_y <- sqrt(var_pred_y)

  var_weighted <- t(w) %*% sigma_y_hat %*% w
  var_error_weighted <- t(w) %*% model$sigma_e %*% w
  se_weighted <- sqrt(var_weighted)
  var_pred_weighted <-var_weighted + var_error_weighted
  se_pred_weighted <- sqrt(var_pred_weighted)
  # var_predictions = diag(sigma_y_hat + diag(sigma_e))
  # var_weighted_prediction = t(w) %*% sigma_y_hat %*% w + t(w) %*% sigma_e %*% w
  # se_weighted_prediction =
  ci_multiplier <- stats::qnorm(0.975)
  predictions <- data.frame(outcome = model$outcomes,
                            .fitted = predictions,
                            .se.fit = se_y,
                            .conf.low = weighted_prediction - (ci_multiplier*se_weighted),
                            .conf.high = weighted_prediction + (ci_multiplier*se_weighted),
                            .se.forecast = se_pred_y,
                            pred.low = predictions - (ci_multiplier*se_pred_y),
                            pred.high = predictions + (ci_multiplier*se_pred_y))

  weighted_prediction <- data.frame(outcome = "weighted",
                                    .fitted = weighted_prediction, .se.fit = se_weighted,
                                    .conf.low = weighted_prediction - (ci_multiplier*se_weighted),
                                    .conf.high = weighted_prediction + (ci_multiplier*se_weighted),
                                    .se.forecast = se_pred_weighted,
                                    pred.low = weighted_prediction - (ci_multiplier*se_pred_weighted),
                                    pred.high = weighted_prediction + (ci_multiplier*se_pred_weighted))

  results <- rbind(predictions, weighted_prediction)
  return(results)
}

get_x_matrix <- function(x, covariates) {
  if(is.null(names(x))){
    x <- c(x,1)
  } else {
    x <- c(x, intercept = 1)
    x <- x[covariates]
  }
  return(matrix(x, ncol = 1))
}
