process_lavaan_estimates <- function(lavaan_fit) {
  beta <- get_beta(lavaan_fit)
  sigma_e <- get_sigma_e(lavaan_fit)
  sigma_beta <- get_sigma_beta(lavaan_fit, beta)

  rescale_factors <- get_rescale_df(model = lavaan_fit)

  return(list(beta = beta, sigma_beta = sigma_beta, sigma_e = sigma_e, rescale_df = rescale_factors))
}

get_lavaan_estimates <- function(lavaan_fit, remove.nonfree) {
  outcomes <- lavaan::lavNames(lavaan_fit, type = "ov.y")
  estimates <- lavaan::parameterEstimates(lavaan_fit, remove.nonfree = remove.nonfree,
                                          se = FALSE,
                                          zstat = FALSE,
                                          pvalue = FALSE,
                                          ci = FALSE)
  estimates <- estimates[estimates$lhs %in% outcomes,]
  return(estimates)
}

get_beta <- function(lavaan_fit) {
  estimates <- get_lavaan_estimates(lavaan_fit, remove.nonfree = TRUE)
  beta <- estimates[(estimates$op == "~" | estimates$op == "~1"), ]
  beta$eqname <- paste0(beta$lhs, beta$op, beta$rhs)
  beta$rhs[beta$op=="~1"] <- "intercept"
  return(beta)
}

get_sigma_e <- function(lavaan_fit) {
  estimates <- get_lavaan_estimates(lavaan_fit, remove.nonfree = FALSE)
  outcomes <- lavaan::lavNames(lavaan_fit, type = "ov.y")
  estimates <- lavaan::parameterEstimates(lavaan_fit, remove.nonfree = FALSE,
                                          se = FALSE,
                                          zstat = FALSE,
                                          pvalue = FALSE,
                                          ci = FALSE)
  estimates <- estimates[estimates$lhs %in% outcomes,]
  error_var_cov <- estimates[estimates$op == "~~",]
  outcomes <- error_var_cov$lhs[error_var_cov$lhs == error_var_cov$rhs]
  sigma_e <- matrix(nrow = length(outcomes), ncol = length(outcomes), dimnames = list(outcomes, outcomes))
  sigma_e[cbind(error_var_cov$lhs, error_var_cov$rhs)] <-  error_var_cov$est
  sigma_e[lower.tri(sigma_e, diag = FALSE)] <- t(sigma_e)[lower.tri(sigma_e, diag = FALSE)]
  return(sigma_e)
}

get_sigma_beta <- function(lavaan_fit, beta) {
  covaraicnce_matrix <- lavaan::vcov(lavaan_fit)
  sigma_beta <- covaraicnce_matrix[beta$eqname, beta$eqname, drop = FALSE]
  return(sigma_beta)
}
