test_that("get_", {
  lavaan_cont <- readRDS("data/lavaan_cont.rds")
  estimates <-   estimates <- lavaan::parameterEstimates(lavaan_cont, remove.nonfree = TRUE,
                                                         se = FALSE,
                                                         zstat = FALSE,
                                                         pvalue = FALSE,
                                                         ci = FALSE)
  beta <- get_beta(estimates)
  expect_equal(dim(beta), c(20, 5))

  sigma_e <- get_sigma_e(estimates)
  expect_equal(dim(sigma_e), c(4, 4))
})

test_that("process_lavaan_estimates", {
  lavaan_cont <- readRDS("data/lavaan_cont.rds")
  processed_estimates <- process_lavaan_estimates(lavaan_cont)
  expect_equal(dim(processed_estimates$sigma_e), c(4,4))
  expect_equal(dim(processed_estimates$beta), c(20, 5))
  expect_equal(dim(processed_estimates$sigma_beta), c(20,20))
})
