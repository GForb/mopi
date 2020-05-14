
#' Import esitmates from stata
#'
#' @param e_b A vector of parameter estimates from a sem model in stata
#' @param e_b_rownames The (long) Stata names of the paramter estimates. Will have format "outcome:parameter".
#' @param e_V The variance/covariance matrix for the parameters
#'
#' @return
#' A list containing the parameter estimates,
#' their covariance matrix, the covariance matrix for the errors,
#' and counts for the number of paramaters and covariates in the model
#'
#' @export
#'
#' @examples
#' load("data/e_V.rda")
#' load("data/e_b.rda")
#' import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)

import_estimates_stata <- function(e_b, e_b_rownames, e_V) {
  beta_sigma_e_and_counts <- process_e_b(e_b, e_b_rownames)
  sigma_beta <- process_e_V(e_V,
                            n_outcomes = beta_sigma_e_and_counts$n_outcomes,
                            n_covariates = beta_sigma_e_and_counts$n_covariates)
  return(c(beta_sigma_e_and_counts, list(sigma_beta = sigma_beta)))
}



process_e_b <- function(e_b, e_b_rownames) {
  parameters <- split_betas_and_errors_stata(e_b, e_b_rownames)
  n_outcomes <- length(parameters$outcomes)
  n_covariates <- length(parameters$covariates)
  beta_t <- matrix(parameters$beta, ncol = n_outcomes)
  beta <- t(beta_t)
  sigma_e <- get_sigma_e(error_variances = parameters$error_variances,
                         error_covariances = parameters$error_covariances,
                         n_outcomes = n_outcomes)
  return(c(list(sigma_e = sigma_e,
                beta = beta, covariates = parameters$covariates,
                outcomes = parameters$outcomes),
                n_outcomes = n_outcomes,
                n_covariates = n_covariates))
}


process_e_V <- function(e_V, n_outcomes, n_covariates) {
  n_betas <- n_outcomes*n_covariates
  beta_covariances <- data.matrix(e_V[1:n_betas, 1:n_betas])
  sigma_beta  <- array(beta_covariances, dim=c(n_covariates, n_covariates ,n_outcomes, n_outcomes))
  for(i in 1:n_outcomes){
    for(j in 1: n_outcomes){
      sigma_beta[, ,i,j] <- beta_covariances[(1 + n_covariates*(i-1)):(n_covariates*i),
                                             (1 + n_covariates*(j-1)):(n_covariates*j)]
    }
  }
  return(sigma_beta)
}

split_betas_and_errors_stata <- function(e_b, e_b_rownames) {
  error_variances <- e_b[grepl("/var*", e_b_rownames)]
  error_covariances <- e_b[grepl("/cov*", e_b_rownames)]
  beta <- e_b[!(grepl("/cov*", e_b_rownames) | grepl("/var*", e_b_rownames))]
  beta_names <- e_b_rownames[!(grepl("/cov*", e_b_rownames) | grepl("/var*", e_b_rownames))]
  outcome_names <- unique(sub("\\:.*", "", beta_names))
  covariate_names <- unique(sub(".*\\:", "", beta_names))
  covariate_names <- sub("_cons", "intercept", covariate_names)
  list(covariates = covariate_names,
       outcomes = outcome_names,
       beta = beta,
       error_variances = error_variances,
       error_covariances = error_covariances)
}



get_sigma_e <- function(error_variances, error_covariances, n_outcomes) {
  sigma_e <- matrix(NA, nrow = n_outcomes, ncol = n_outcomes)
  diag(sigma_e) <-  error_variances
  sigma_e[lower.tri(sigma_e, diag = FALSE)] <- error_covariances
  sigma_e <- t(sigma_e)
  sigma_e[lower.tri(sigma_e, diag = FALSE)] <- error_covariances
  sigma_e
}
