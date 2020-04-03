predict_weighted <- function(data, weights, model_parameters){
  predictions <- apply(X, MARGIN = 1, single_weighted_prediction, weights, model)
  process(predictions)
}

single_weighted_predction <- function(x, weights, model) {
  n_outcomes <- model$n_outcomes
  x <- matrix(c(x,1), ncol = 1)
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
  var_weighted <- t(w) %*% sigma_y_hat %*% w
  se_weighted <- sqrt(var_weighted)

  # var_predictions = diag(sigma_y_hat + diag(sigma_e))
  # var_weighted_prediction = t(w) %*% sigma_y_hat %*% w + t(w) %*% sigma_e %*% w
  # se_weighted_prediction =
  return(list(predictions = predictions,
              weighted_prediction = weighted_prediction,
              sigma_y_hat = sigma_y_hat,
              se_y = se_y,
              se_weighted = se_weighted))
}
