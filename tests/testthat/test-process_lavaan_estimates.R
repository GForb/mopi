test_that("get_lavaan_estimates", {
  lavaan_cont <- readRDS("data/lavaan_cont.rds")
  estimates <- get_lavaan_estimates(lavaan_cont, remove.nonfree = FALSE)
  expect_equal(dim(estimates), c(30, 4))
  estimates <- get_lavaan_estimates(lavaan_cont, remove.nonfree = TRUE)
  expect_equal(dim(estimates), c(30, 4))

  lavaan_cont_and_ordinal <- readRDS("data/lavaan_cont_and_ordinal.rds")
  estimates_ord <- get_lavaan_estimates(lavaan_cont_and_ordinal, remove.nonfree = FALSE)
  expect_equal(dim(estimates_ord), c(24, 4))
  estimates_ord <- get_lavaan_estimates(lavaan_cont_and_ordinal, remove.nonfree = TRUE)
  expect_equal(dim(estimates_ord), c(21, 4))
})

test_that("get_", {
  lavaan_cont <- readRDS("data/lavaan_cont.rds")
  beta <- get_beta(lavaan_cont)
  expect_equal(dim(beta), c(20, 5))

  sigma_e <- get_sigma_e(lavaan_cont)
  expect_equal(dim(sigma_e), c(4, 4))

  sigma_beta <- get_sigma_beta(lavaan_cont, beta)
  expect_equal(dim(sigma_beta), c(20, 20))

  lavaan_cont_and_ordinal <- readRDS("data/lavaan_cont_and_ordinal.rds")

  beta <- get_beta(lavaan_cont_and_ordinal)
  expect_equal(dim(beta), c(14, 5))

  sigma_e <- get_sigma_e(lavaan_cont_and_ordinal)
  expect_equal(dim(sigma_e), c(3, 3))

  sigma_beta <- get_sigma_beta(lavaan_cont_and_ordinal, beta)
  expect_equal(dim(sigma_beta), c(14, 14))

  simple_model <- readRDS("data/simple_model.rds")
  beta <- get_beta(simple_model)
  expect_equal(dim(beta), c(1, 5))

  sigma_e <- get_sigma_e(simple_model)
  expect_equal(dim(sigma_e), c(1, 1))

  sigma_beta <- get_sigma_beta(simple_model, beta)
  expect_equal(dim(sigma_beta), c(1, 1))

})

test_that("process_lavaan_estimates", {
  lavaan_cont <- readRDS("data/lavaan_cont.rds")
  processed_estimates <- process_lavaan_estimates(lavaan_cont)
  expect_equal(dim(processed_estimates$sigma_e), c(4,4))
  expect_equal(dim(processed_estimates$beta), c(20, 5))
  expect_equal(dim(processed_estimates$sigma_beta), c(20,20))
  expect_equal(dim(processed_estimates$rescale_df), c(4, 4))

  lavaan_cont_and_ordinal <- readRDS("data/lavaan_cont_and_ordinal.rds")
  processed_estimates <- process_lavaan_estimates(lavaan_cont_and_ordinal)
  expect_equal(dim(processed_estimates$sigma_e), c(3,3))
  expect_equal(dim(processed_estimates$beta), c(14, 5))
  expect_equal(dim(processed_estimates$sigma_beta), c(14,14))
  expect_equal(dim(processed_estimates$rescale_df), c(3, 4))

})
