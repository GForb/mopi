get_rescale_df  <- function(model) {
  outcomes <- lavaan::lavNames(model, type = "ov.y")
  ordinal_outcomes <- get_ordinal_outcomes(model)
  rescale_df <- purrr::map_dfr(outcomes,
                               get_scale_factor,
                               model = model,
                               ordinal_outcomes = ordinal_outcomes)
  return(rescale_df)
}


get_ordinal_outcomes <- function(lavaan_model) {
  ordinal_outcomes <- lavaan::lavNames(lavaan_model, type = "ov.ord")
  return(ordinal_outcomes)
}

get_scale_factor <- function(outcome, model, ordinal_outcomes) {
  if(outcome %in% ordinal_outcomes){
    prediction <- predict_outcome_lavaan(model, outcome)
    scale_factors <- data.frame(outcome = outcome,
                                mean = mean(prediction),
                                variance = var(prediction),
                                scale = 1/sqrt(1 + var(prediction)))
  } else {
    scale_factors <- data.frame(outcome = outcome,
                                mean = 0,
                                variance = NA,
                                scale = 1)
  }
  return(scale_factors)
}

predict_outcome_lavaan <- function(model, outcome) {
  covariates <- get_covariates(model, outcome)
  covaraite_names <- covariates$rhs
  cov_data <- get_covariate_data(model, covaraite_names)
  cov_data <- data.matrix(cov_data)
  beta <- matrix(covariates$est[covaraite_names == colnames(cov_data)], ncol = 1)
  predictions <- cov_data %*% beta

  return(predictions)
}

get_covariates <- function(model, outcome) {
  estimates <- lavaan::parameterEstimates(model, remove.nonfree = TRUE,
                                          se = FALSE,
                                          zstat = FALSE,
                                          pvalue = FALSE,
                                          ci = FALSE)
  beta <- estimates[(estimates$op == "~" | estimates$op == "~1"), ]
  outcome_beta <- beta[beta$lhs == outcome,]
  return(outcome_beta)
}

get_covariate_data <- function(model, covariates) {
  names <- lavaan::lavNames(model)
  data <- lavaan::lavTech(model, what = "data")[[1]]
  covariate_data <- data[, names %in% covariates, drop=FALSE]
  colnames(covariate_data) <- names[names %in% covariates]
  return(covariate_data)
}





