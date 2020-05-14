

import_estimates_mplus <- function(mplus_output, tech3, mplus_results) {
  counts <- counts_and_names_mplus(mplus_output)
  betas_and_errors <- split_betas_and_errors_mplus(mplus_results = mplus_results,
                                                 n_outcomes = counts$n_outcomes,
                                                 n_covariates = counts$n_covariates)
  beta <- process_betas_mplus(betas_and_errors$betas, counts$n_outcomes, counts$n_covariates)
  sigma_e <- process_errors_mplus(betas_and_errors$errors, counts$n_outcomes)
  sigma_beta <- process_beta_covariances_mplus(tech3,
                                               n_covariates = counts$n_covariates,
                                               n_outcomes =  counts$n_outcomes)
  list(beta = beta,
       sigma_e = sigma_e,
       sigma_beta = sigma_beta,
       n_outcomes = counts$n_outcomes,
       n_covariates = counts$n_covariates,
       covariates = counts$covariate_names,
       outcomes = counts$outcome_names)
}

counts_and_names_mplus <- function(mplus_output) {
  param_spec <- mplus_output$tech1$parameterSpecification
  parameter_table <- mplus_output$parameters$unstandardized
  variable_names <- parameter_table$param
  outcome_names <- tolower(variable_names[parameter_table$paramHeader == "Intercepts"])
  covariate_names <- unique(variable_names[!variable_names %in% toupper(outcome_names)])
  covariate_names <- c(tolower(covariate_names), "intercept")
  n_outcomes = max(param_spec$alpha)
  n_covariates = (max(param_spec$beta))/n_outcomes
  n_parameters = nrow(mplus_output$parameters$unstandardized)
  return(list(n_outcomes = n_outcomes,
              n_covariates = n_covariates,
              n_parameters = n_parameters,
              outcome_names = outcome_names,
              covariate_names = covariate_names))
}

split_betas_and_errors_mplus <- function(mplus_results, n_outcomes, n_covariates) {
  n_betas <- n_outcomes *n_covariates
  n_errors <- 0.5*n_outcomes*(n_outcomes + 1)
  mplus_results_vector <- as.vector(t(data.matrix(mplus_results)) )
  betas <- mplus_results_vector[1:n_betas]
  errors <- mplus_results_vector[n_betas + 1: n_errors]
  return(list(betas = betas, errors = errors))
}

process_betas_mplus <- function(betas, n_outcomes, n_covariates) {
  intercepts <- betas[1:n_outcomes]
  non_intercepts <- betas[n_outcomes+1: length(betas)]
  beta <- t(matrix(non_intercepts, nrow = n_outcomes , ncol = n_covariates -1))
  beta <- cbind(beta, intercepts)
  return(beta = unname(beta))
}

process_errors_mplus <- function(errors, n_outcomes) {
  sigma_e <- matrix(nrow = n_outcomes, ncol = n_outcomes)

  sigma_e[upper.tri(sigma_e, diag = TRUE)] <- errors
  sigma_e[lower.tri(sigma_e, diag = FALSE)] <- t(sigma_e)[lower.tri(sigma_e, diag = FALSE)]

  return(sigma_e)
}



process_beta_covariances_mplus <- function(tech3, n_covariates, n_outcomes) {
  param_covariances <- process_tech3(tech3)
  n_betas <- n_covariates*n_outcomes
  beta_covariances <- param_covariances[1:n_betas, 1:n_betas]
  beta_covariances_modified <- matrix(nrow = n_betas, ncol = n_betas)
  for(i in 1:n_betas){
    for(j in 1:n_betas){
      beta_covariances_modified[i,j] <- beta_covariances[reindex_beta_covariance(i, n_outcomes, n_covariates),
                                            reindex_beta_covariance(j, n_outcomes, n_covariates)]
    }
  }
  sigma_beta  <- array(beta_covariances, dim=c(n_covariates, n_covariates ,n_outcomes, n_outcomes))
  for(i in 1:n_outcomes){
    for(j in 1: n_outcomes){
      sigma_beta[, ,i,j] <- beta_covariances_modified[(1 + n_covariates*(i-1)):(n_covariates*i),
                                             (1 + n_covariates*(j-1)):(n_covariates*j)]
    }
  }
  return(sigma_beta)
}





process_tech3 <- function(tech3) {
  tech3_mat1 <- t(data.matrix(tech3))
  tech3_vec <- as.vector(tech3_mat1)
  n_parameters = triangle_root(length(tech3_vec))
  # note tech3 is the lower triangle of the covariance matrix (including the diaganal).
  # The number of parameters is the triangle root of the length of tech3 ie. the number n such that
  # length tech3 is the nth triangle number

  tech3_mat <- matrix(nrow = n_parameters, ncol = n_parameters)
  tech3_mat[upper.tri(tech3_mat, diag = TRUE)] <- tech3_vec
  tech3_mat[lower.tri(tech3_mat, diag = FALSE)] <- t(tech3_mat)[lower.tri(tech3_mat, diag = FALSE)]
  return(tech3_mat)
}

triangle_root <- function(x) {
  return((sqrt(8*x +1) -1)/2)
}



reindex_beta_covariance <- function(index, n_outcomes, n_covariates) {
  n_betas <- n_outcomes*n_covariates
  if(index%%n_covariates == 0) {
    new_index <- index/n_covariates
  } else {
    new_index <- index + n_outcomes - floor(index/n_covariates)
  }
  return(new_index)
}
