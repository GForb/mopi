process_lavaan_estimates <- function(lavaan_fit) {
  estimates <- lavaan::parameterEstimates(lavaan_fit, remove.nonfree = TRUE,
                                          se = FALSE,
                                          zstat = FALSE,
                                          pvalue = FALSE,
                                          ci = FALSE)
  beta <- get_beta(estimates)
  sigma_e <- get_sigma_e(estimates)

  sigma_beta <- lavaan::vcov(lavaan_fit)[beta$eqname, beta$eqname]
  return(list(beta = beta, sigma_beta = sigma_beta, sigma_e = sigma_e))
}

get_beta <- function(estimates) {
  beta <- estimates[(estimates$op == "~" | estimates$op == "~1"), ]
  beta$eqname <- paste0(beta$lhs, beta$op, beta$rhs)
  beta$rhs[beta$op=="~1"] <- "intercept"
  return(beta)
}

get_sigma_e <- function(estimates) {
  error_var_cov <- estimates[estimates$op == "~~",]
  outcomes <- error_var_cov$lhs[error_var_cov$lhs == error_var_cov$rhs]
  sigma_e <- matrix(nrow = length(outcomes), ncol = length(outcomes), dimnames = list(outcomes, outcomes))
  sigma_e[cbind(error_var_cov$lhs, error_var_cov$rhs)] <-  error_var_cov$est
  sigma_e[lower.tri(sigma_e, diag = FALSE)] <- t(sigma_e)[lower.tri(sigma_e, diag = FALSE)]
  return(sigma_e)
}
