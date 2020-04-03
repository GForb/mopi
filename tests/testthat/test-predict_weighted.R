context("Predict weighted")

test_that("single_weighted_predction", {
  load("data/e_V.rda")
  load("data/e_b.rda")
  model_estimates <- import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)
  results <- single_weighted_predction(x = c(1,1), weights = c(1,1), model = model_estimates)
  predictions <- results$predictions
  expect_equal(length(predictions), model_estimates$n_outcomes)
  expect_lt(predictions[[1]]- 1.848806, 0.00001)
  expect_lt(predictions[[2]]- 1.971099 , 0.00001)

  weighted_prediction <- results$weighted_prediction
  expect_lt(weighted_prediction- 3.819905 , 0.00001)

  sigma_y <- results$sigma_y_hat
  expect_equal(dim(sigma_y), c(2,2))

  se_y <- results$se_y
  expect_equal(length(se_y), 2)
  expect_lt(se_y[[1]]- 0.2456221 , 0.00001)
  expect_lt(se_y[[2]]- 0.2218644 , 0.00001)

  se_w <- results$se_weighted
  expect_equal(length(se_w), 1)
  expect_lt(se_w - 0.4318444 , 0.00001)
})
