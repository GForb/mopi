context("Import estimates")



test_that("split_betas_and_errors", {
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  expect_equal(length(betas_and_errors), 4)
  expect_equal(length(betas_and_errors$beta), 6)
  expect_equal(length(betas_and_errors$beta_names), 6)
  expect_equal(length(betas_and_errors$error_variances), 2)
  expect_equal(length(betas_and_errors$error_covariances), 1)
})

test_that("count_variables_stata", {
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  counts <- count_variables_stata(betas_and_errors$beta_names)
  expect_equal(counts$n_outcomes, 2)
  expect_equal(counts$n_covariates, 3)
  expect_equal(counts$outcomes, c("stand_y1", "stand_y2"))
})

test_that("get_sigma_e", {
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  counts <- count_variables_stata(betas_and_errors$beta_names)
  sigma_e <- get_sigma_e(error_variances = betas_and_errors$error_variances,
              error_covariances = betas_and_errors$error_covariances,
              n_outcomes = counts$n_outcomes)
  expect_equal(dim(sigma_e), c(2, 2))
  expect_true(isSymmetric(sigma_e))
})

test_that("process_e_b", {
  load("data/e_b.rda")
  processed_beta <- process_e_b(e_b[[1]], e_b[[2]])
  expect_equal(dim(processed_beta$beta), c(processed_beta$n_outcomes, processed_beta$n_covariates))
  expect_equal(dim(processed_beta$sigma_e), c(processed_beta$n_outcomes, processed_beta$n_outcomes))
  expect_equal(processed_beta$n_outcomes, 2)
  expect_equal(processed_beta$n_covariates, 3)
  expect_equal(processed_beta$outcomes, c("stand_y1", "stand_y2"))
})

test_that("process_e_V", {
  load("data/e_V.rda")
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  counts <- count_variables_stata(betas_and_errors$beta_names)
  sigma_beta <- process_e_V(e_V,
                            n_outcomes = counts$n_outcomes,
                            n_covariates = counts$n_covariates)
  expect_equal(dim(sigma_beta), c(counts$n_covariate, counts$n_covariate, counts$n_outcomes, counts$n_outcomes))
})

test_that("process_e_V", {
  load("data/e_V.rda")
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  counts <- count_variables_stata(betas_and_errors$beta_names)
  sigma_beta <- process_e_V(e_V,
                            n_outcomes = counts$n_outcomes,
                            n_covariates = counts$n_covariates)
  expect_equal(dim(sigma_beta), c(counts$n_covariate, counts$n_covariate, counts$n_outcomes, counts$n_outcomes))
})

test_that("import_estimates_stata", {
  load("data/e_V.rda")
  load("data/e_b.rda")
  estimates <- import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)
  expect_equal(estimates$n_outcomes, 2)
  expect_equal(estimates$n_covariates, 3)
  expect_equal(estimates$outcomes, c("stand_y1", "stand_y2"))
  expect_equal(length(estimates$beta), estimates$n_outcomes*estimates$n_covariates)
  expect_equal(dim(estimates$sigma_e), c(estimates$n_outcomes,estimates$n_outcomes))
  expect_equal(dim(estimates$sigma_beta), c(estimates$n_covariate,
                                            estimates$n_covariate,
                                            estimates$n_outcomes,
                                            estimates$n_outcomes))
})


