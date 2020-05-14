test_that("plot_predictions", {
  library(ggplot2)
  load("data/test_plot.rda")
  load("data/e_V.rda")
  load("data/e_b.rda")


  model_estimates <- import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)
  new_x <- c(bestviq2 = 22 ,
             bestnviq2 = 51.5 ,
             css2 =6,
             age2_vdlst = 61.5)
  results <- single_weighted_predction(x = new_x, weights = rep(0.25,4), model = model_estimates)
  plot <- plot_predictions(results)
  plot
  expect_equal(1,1)
})
