test_that("plot_predictions", {
  library(ggplot2)
  lavaan_cont <- readRDS("data/lavaan_cont.rds")

  processed_estimates <- process_lavaan_estimates(lavaan_cont)
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
                                     processed_lavaan = processed_estimates)
  plot <- plot_predictions(results)
  plot
  expect_equal(1,1)
})
