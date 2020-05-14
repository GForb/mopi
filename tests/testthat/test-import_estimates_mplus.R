test_that("counts_and_names_mplus", {
  load("data/mplus_output.rda")
  counts <- counts_and_names_mplus(mplus_output)
  expect_equal(counts$n_outcomes, 4)
  expect_equal(counts$n_covariates, 5)
  expect_equal(counts$outcome_names, c("stand_y1", "stand_y2", "stand_y3", "stand_y4"))
  expect_equal(counts$covariate_names, c("bestviq2", "bestnviq2", "age2_vdlst", "css2", "intercept"))
})

test_that("split_betas_and_errors_mplus", {
  load("data/mplus_output.rda")
  load("data/mplus_results.rda")

  counts <- counts_and_names_mplus(mplus_output)
  betas_and_errors <- split_betas_and_errors_mplus(mplus_results, counts$n_outcomes, counts$n_covariates)
  expect_equal(length(betas_and_errors$betas), 20)
  expect_equal(length(betas_and_errors$errors), 10)
})

test_that("process_betas_mplus", {
  load("data/mplus_output.rda")
  load("data/mplus_results.rda")
  counts <- counts_and_names_mplus(mplus_output)
  betas_and_errors <- split_betas_and_errors_mplus(mplus_results, counts$n_outcomes, counts$n_covariates)
  processed_beta <- process_betas_mplus(betas_and_errors$betas, counts$n_outcomes, counts$n_covariates)
  expect_equal(dim(processed_beta), c(4,5))

  load("data/e_b.rda")
  processed_beta_stata <- process_e_b(e_b[[1]], e_b[[2]])
  expect_equal(processed_beta_stata$beta, processed_beta, tolerance = 0.000001)
})

test_that("process_errors_mplus", {
  load("data/mplus_output.rda")
  load("data/mplus_results.rda")
  counts <- counts_and_names_mplus(mplus_output)
  betas_and_errors <- split_betas_and_errors_mplus(mplus_results, counts$n_outcomes, counts$n_covariates)
  errors <- process_errors_mplus(betas_and_errors$errors, counts$n_outcomes)

  expect_equal(dim(errors), c(4,4))

  load("data/e_b.rda")
  processed_beta_stata <- process_e_b(e_b[[1]], e_b[[2]])
  expect_equal(processed_beta_stata$sigma_e, errors, tolerance = 0.000001)
})

test_that("process_tech3", {
    load("data/tech3.rda")
    processed_tech3 <- process_tech3(tech3)
    expect_equal(dim(processed_tech3), c(30,30))
    expect_equal(processed_tech3, t(processed_tech3))
})

test_that("process_beta_covariances_mplus", {
  load("data/mplus_output.rda")
  load("data/tech3.rda")

  counts <- counts_and_names_mplus(mplus_output)
  sigma_beta <- process_beta_covariances_mplus(tech3, counts$n_covariates, counts$n_outcomes)

  expect_equal(dim(sigma_beta), c(5,5,4,4))

  load("data/e_V.rda")
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  n_outcomes <- length(betas_and_errors$outcomes)
  n_covariates <- length(betas_and_errors$covariates)
  sigma_beta_stata <- process_e_V(e_V,
                            n_outcomes  = n_outcomes,
                            n_covariates = n_covariates)

  expect_equal(sigma_beta, sigma_beta_stata, tolerance = 0.000001)
})

test_that("import_estimates_mplus", {
  load("data/e_V.rda")
  load("data/e_b.rda")
  stata_estimates <- import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)

  load("data/mplus_output.rda")
  load("data/tech3.rda")
  load("data/mplus_results.rda")

  mplus_estimates <- import_estimates_mplus(mplus_output, tech3, mplus_results)
  expect_equal(mplus_estimates$beta, stata_estimates$beta, tolerance = 0.000001)
  expect_equal(mplus_estimates$sigma_e, stata_estimates$sigma_e, tolerance = 0.000001)
  expect_equal(mplus_estimates$sigma_beta, stata_estimates$sigma_beta, tolerance = 0.000001)
  expect_equal(mplus_estimates$n_outcomes, stata_estimates$n_outcomes)
  expect_equal(mplus_estimates$n_covariates, stata_estimates$n_covariates)
  expect_equal(mplus_estimates$covariates, stata_estimates$covariates)
  expect_equal(mplus_estimates$outcomes, stata_estimates$outcomes)
})
