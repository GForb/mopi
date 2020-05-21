test_that("plot_predictions", {
  library(ggplot2)

  # Continuous outcomes only
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
                                     lavaan_model =  lavaan_cont)
  plot <- plot_predictions(results)
  plot

 # Including ordinal outcomes
  model_ord <- readRDS("data/lavaan_cont_and_ordinal.rds")
  weights <- c(stand_y1 = 0.25,
               stand_y2 = 0.25,
               y13 = 0.5)
  results2 <- predict_weighted_lavaan(new_x = new_x,
                                      weights = weights,
                                      lavaan_model = model_ord)
  plot_ord <- plot_predictions(results2)
  plot_ord

  #Test that always passes
  expect_equal(1,1)
})
