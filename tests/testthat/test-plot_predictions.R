context("plot_predictions")
test_that("plot_predictions", {
  load("data/test_plot.rda")
  load("data/e_V.rda")
  load("data/e_b.rda")


  model_estimates <- import_estimates_stata(e_b = e_b[[1]], e_b_rownames = e_b[[2]], e_V = e_V)
  results <- single_weighted_predction(x = c(1,1), weights = c(0.5,0.5), model = model_estimates)
  plot <- plot_predictions(results)
  plot
  #expect_silent(plot)
  #vdiffr::expect_doppelganger(plot, test_plot)
})
