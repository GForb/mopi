test_that("split_betas_and_errors_stata", {
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  expect_equal(length(betas_and_errors), 5)
  expect_equal(length(betas_and_errors$beta), 20)
  expect_equal(length(betas_and_errors$outcomes), 4)
  expect_equal(length(betas_and_errors$covariates), 5)
  expect_equal(length(betas_and_errors$error_variances), 4)
  expect_equal(length(betas_and_errors$error_covariances), 6)
})


test_that("get_sigma_e", {
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  sigma_e <- get_sigma_e(error_variances = betas_and_errors$error_variances,
              error_covariances = betas_and_errors$error_covariances,
              n_outcomes = length(betas_and_errors$outcomes))
  expect_equal(dim(sigma_e), c(4, 4))
  expect_true(isSymmetric(sigma_e))
})

test_that("process_e_b", {
  load("data/e_b.rda")
  processed_beta <- process_e_b(e_b[[1]], e_b[[2]])
  expect_equal(dim(processed_beta$beta), c(processed_beta$n_outcomes, processed_beta$n_covariates))
  expect_equal(dim(processed_beta$sigma_e), c(processed_beta$n_outcomes, processed_beta$n_outcomes))
  expect_equal(processed_beta$n_outcomes, 4)
  expect_equal(processed_beta$n_covariates, 5)
  expect_equal(processed_beta$outcomes, c("stand_y1", "stand_y2", "stand_y3", "stand_y4"))
  expect_equal(processed_beta$covariates, c("bestviq2", "bestnviq2", "age2_vdlst", "css2", "intercept"))
})

test_that("process_e_V", {
  load("data/e_V.rda")
  load("data/e_b.rda")
  betas_and_errors <- split_betas_and_errors_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]])
  n_outcomes <- length(betas_and_errors$outcomes)
  n_covariates <- length(betas_and_errors$covariates)
  sigma_beta <- process_e_V(e_V,
                            n_outcomes  = n_outcomes,
                            n_covariates = n_covariates)
  expect_equal(dim(sigma_beta), c(n_covariates, n_covariates, n_outcomes, n_outcomes))
})



test_that("import_estimates_stata", {
  load("data/e_V.rda")
  load("data/e_b.rda")
  estimates <- import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)
  expect_equal(estimates$n_outcomes, 4)
  expect_equal(estimates$n_covariates, 5)
  expect_equal(estimates$outcomes, c("stand_y1", "stand_y2", "stand_y3", "stand_y4"))
  expect_equal(estimates$covariates, c("bestviq2", "bestnviq2", "age2_vdlst", "css2", "intercept"))

  expect_equal(length(estimates$beta), estimates$n_outcomes*estimates$n_covariates)
  expect_equal(dim(estimates$sigma_e), c(estimates$n_outcomes,estimates$n_outcomes))
  expect_equal(dim(estimates$sigma_beta), c(estimates$n_covariate,
                                            estimates$n_covariate,
                                            estimates$n_outcomes,
                                            estimates$n_outcomes))
  stata_error_covariance <- matrix(nrow = 4, ncol = 4)
  diag(stata_error_covariance) <-  c(.4661082,  .4967597, .5248603,  .7958349)
  stata_error_covariance[lower.tri(stata_error_covariance, diag = FALSE)] <- c(.3433601, .3745297, .2561671,  .451017,  .244027, .2484536)
  stata_error_covariance[upper.tri(stata_error_covariance, diag = FALSE)] <- t(stata_error_covariance)[upper.tri(stata_error_covariance, diag = FALSE)]
  expect_equal(estimates$sigma_e, stata_error_covariance, tolerance = 0.000001)

  estimates$sigma_beta

  sigma_beta_1_1 <- matrix(nrow = 5, ncol = 5)
  sigma_beta_1_1[upper.tri(sigma_beta_1_1, diag = TRUE)] <-
     c(.00003149,
    -9.952e-06,      .00002899,
    -.00001383,     -.00004237,      .00021186,
    .00012629,      3.376e-06 ,    -.00008574 ,     .00133342,
    -.00049637,      .00123756,     -.01032442,     -.00852692 ,      .7012481)
  sigma_beta_1_1[lower.tri(sigma_beta_1_1, diag = FALSE)] <-
    t(sigma_beta_1_1)[lower.tri(sigma_beta_1_1, diag = FALSE)]
  expect_equal(sigma_beta_1_1, estimates$sigma_beta[,,1,1], tolerance = 0.000001)
  sigma_beta_4_1 <-
    matrix(c(.00001731,     -5.470e-06,     -7.602e-06,      .00006941,      -.0002728,
             -5.470e-06,      .00001593,     -.00002328,      1.855e-06,      .00068015,
             -7.602e-06,     -.00002328,      .00011644,     -.00004712,     -.00567417,
             .00006941,      1.855e-06,     -.00004712,      .00073283,     -.00468629,
             -.0002728,      .00068015,     -.00567417,     -.00468629,      .38539701), nrow = 5)

  expect_equal(sigma_beta_4_1, estimates$sigma_beta[,,4,1], tolerance = 0.000001)


})


