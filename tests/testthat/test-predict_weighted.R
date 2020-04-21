context("Predict weighted")

test_that("single_weighted_predction", {
  load("data/e_V.rda")
  load("data/e_b.rda")


  model_estimates <- import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)
  results <- single_weighted_predction(x = c(1,1), weights = c(0.5,0.5), model = model_estimates)

  expect_equal(dim(results), c(3, 8))
  expect_lt(abs(results$.fitted[1] - 1.848806),0.00001)
  expect_lt(abs(results$.fitted[2] - 1.971099),0.00001)
  expect_lt(abs(results$.fitted[3] - 1.909953),0.00001)
  expect_lt(abs(results$.se.fit[1] - 0.2456221),0.00001)
  expect_lt(abs(results$.se.fit[2] - 0.2218644),0.00001)
  expect_lt(abs(results$.se.fit[3] - 0.2159222),0.00001)
  expect_true(all(results$pred.low < results$pred.high))
  expect_true(all(results$.conf.low < results$.conf.high))
  expect_equal(results$outcome, factor(c("stand_y1", "stand_y2", "weighted")))
})
