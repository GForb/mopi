test_that("predict_single_outcome", {
  lavaan_cont <- readRDS("data/lavaan_cont.rds")

  processed_estimates <- process_lavaan_estimates(lavaan_cont)
  new_x <- c(bestviq2 = 22 ,
             bestnviq2 = 51.5 ,
             css2 =6,
             vdlst2 = 61.5,
             intercept = 1)
  outcome_name <- "stand_y1"
  results <- predict_single_outcome(outcome_name, new_x, processed_estimates)

  expect_equal(results$.fitted, 0.7099813, tolerance = 0.0001)
  expect_equal(results$.se.fit, 0.1409721, tolerance = 0.0001)
  expect_equal(results$outcome, "stand_y1")
  expect_true(results$.pred.low < results$.pred.high)
  expect_true(results$.conf.low < results$.conf.high)
})


test_that("predict_outcomes_lavaan", {
  lavaan_cont <- readRDS("data/lavaan_cont.rds")

  processed_estimates <- process_lavaan_estimates(lavaan_cont)
  new_x <- c(bestviq2 = 22 ,
             bestnviq2 = 51.5 ,
             css2 =6,
             vdlst2 = 61.5,
             intercept = 1)
  results <- predict_outcomes_lavaan(new_x = new_x, processed_lavaan = processed_estimates)
  expect_equal(dim(results), c(4, 8))
  expect_equal(results$.fitted, c(0.7099813, 0.7939842 ,0.834669 ,0.3063492), tolerance = 0.00001)
  expect_equal(results$.se.fit, c(0.1409721, 0.1456348 ,0.1496972 ,0.1843331),tolerance = 0.001)

  expect_true(all(results$pred.low < results$pred.high))
  expect_true(all(results$.conf.low < results$.conf.high))
  expect_equal(results$outcome, c("stand_y1", "stand_y2","stand_y3", "stand_y4"))
})



test_that("prediction_intervals", {
  model_cont <-  readRDS("data/lavaan_cont.rds")
  new_x <- c(bestviq2 = 22 ,
             bestnviq2 = 51.5 ,
             css2 =6,
             vdlst2 = 61.5,
             intercept = 1)
  weights <- c(stand_y1 = 0.25,
               stand_y2 = 0.25,
               stand_y3 = 0.25,
               stand_y4 = 0.25)

  model_cont <-  readRDS("data/lavaan_cont.rds")
  estimates <- process_lavaan_estimates(model_cont)
  prediction_matricies_ord <- get_args_weighted(new_x = new_x,
                                                weights = weights,
                                                processed_lavaan = estimates)
  results <- do.call(prediction_intervals, prediction_matricies_ord)
  expect_equal(dim(results), c(1, 7))
  expect_equal(results$.fitted, 0.6612459, tolerance = 0.00001)
  expect_equal(results$.se.fit, 0.1277793,tolerance = 0.001)

  expect_true(results$.pred.low < results$.pred.high)
  expect_true(results$.conf.low < results$.conf.high)
})

test_that("predict_weighted_lavaan", {
  lavaan_cont <- readRDS("data/lavaan_cont.rds")

  new_x <- c(bestviq2 = 22 ,
             bestnviq2 = 51.5 ,
             css2 =6,
             vdlst2 = 61.5)
  weights <- c(stand_y1 = 0.25,
               stand_y2 = 0.25,
               stand_y3 = 0.25,
               stand_y4 = 0.25)

  results <- predict_weighted_lavaan(new_x = new_x,
                                     weights = weights,
                                     lavaan_model = lavaan_cont,
                                     predict_outcomes = FALSE)

  expect_equal(dim(results), c(1, 8))
  expect_equal(results$.fitted, 0.6612459, tolerance = 0.00001)
  expect_equal(results$.se.fit, 0.1277793,tolerance = 0.001)

  expect_true(results$.pred.low < results$.pred.high)
  expect_true(results$.conf.low < results$.conf.high)
  expect_equal(results$outcome, "weighted")

  results2 <- predict_weighted_lavaan(new_x = new_x,
                                     weights = weights,
                                     lavaan_model = lavaan_cont)

  expect_equal(dim(results2), c(5, 8))
  expect_equal(results2$.fitted, c(0.7099813, 0.7939842 ,0.834669 ,0.3063492 ,0.6612459), tolerance = 0.00001)

  expect_true(all(results2$pred.low < results$pred.high))
  expect_true(all(results2$.conf.low < results$.conf.high))
  expect_equal(results2$outcome, c("stand_y1", "stand_y2","stand_y3", "stand_y4", "weighted"))

})


test_that("predict_weighted_lavaan_numerical_results_ordinal", {
  ac_analysis_data2 <- read.csv("Data/autism_cohort_analysis2.csv")
  ac_analysis_data2 <- ac_analysis_data2 %>% dplyr::mutate(y13 = ordered(y13), y14 = ordered(y14))

  ord_model1 <- 'y13 ~ bestviq2'
  ord_model2 <- 'y13 + y14 ~ bestviq2
                y13 ~~ y14'

  ord_fit  <- lapply(list(ord_model1, ord_model2),
                     sem,
                     data=ac_analysis_data2)

  new_x <- c(bestviq2 = 22)
  weights <- c(y13  = 0.25,
               y14 = 0.75)
  results1 <- predict_weighted_lavaan(new_x = new_x,
                                      weights = weights,
                                      lavaan_model = ord_fit[[1]])

})
