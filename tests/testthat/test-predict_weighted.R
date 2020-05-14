test_that("single_weighted_predction", {
  load("data/e_V.rda")
  load("data/e_b.rda")


  model_estimates <- import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)
  new_x <- c(bestviq2 = 22 ,
             bestnviq2 = 51.5 ,
             css2 =6,
             age2_vdlst = 61.5)
  results <- single_weighted_predction(x = new_x, weights = rep(0.25,4), model = model_estimates)

  expect_equal(dim(results), c(5, 8))
  expect_equal(results$.fitted, c(0.7099813, 0.7939842 ,0.834669 ,0.3063492 ,0.6612459), tolerance = 0.00001)
  expect_equal(results$.se.fit, c(0.1409721, 0.1456348 ,0.1496972 ,0.1843331 ,0.1277793),tolerance = 0.001)

  expect_true(all(results$pred.low < results$pred.high))
  expect_true(all(results$.conf.low < results$.conf.high))
  expect_equal(results$outcome, factor(c("stand_y1", "stand_y2","stand_y3", "stand_y4", "weighted")))

  load("data/mplus_output.rda")
  load("data/tech3.rda")
  load("data/mplus_results.rda")

  model_estimates_mplus <- import_estimates_mplus(mplus_output, tech3, mplus_results)
  results_mplus <- single_weighted_predction(x = new_x, weights = rep(0.25,4), model = model_estimates_mplus)
  expect_equal(dim(results_mplus), c(5, 8))
  expect_equal(results_mplus$.fitted, c(0.7099813, 0.7939842 ,0.834669 ,0.3063492 ,0.6612459), tolerance = 0.00001)
  expect_equal(results_mplus$.se.fit, c(0.1409721, 0.1456348 ,0.1496972 ,0.1843331 ,0.1277793),tolerance = 0.0001)

  expect_true(all(results_mplus$pred.low < results$pred.high))
  expect_true(all(results_mplus$.conf.low < results$.conf.high))
  expect_equal(results_mplus$outcome, factor(c("stand_y1", "stand_y2","stand_y3", "stand_y4", "weighted")))


})
