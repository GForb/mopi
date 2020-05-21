test_that("get_rescale_df", {
  model <- readRDS("data/lavaan_cont_and_ordinal.rds")
  scale_factors_ref <- data.frame(outcome = c("stand_y1", "stand_y2", "y13"),
                                  mean = c(0, 0, -2.448692),
                                  variance = c(NA, NA, 0.6474245),
                                  scale = c(1, 1, 0.7791072))
  scale_factors <- get_rescale_df(model)
  expect_equal(scale_factors, scale_factors_ref, tolerance = 0.000001)

  lavaan_cont <- readRDS("data/lavaan_cont.rds")

  scale_factors_ref <- data.frame(outcome = c("stand_y1", "stand_y2", "stand_y3", "stand_y4"),
                                  mean = c(0, 0, 0, 0),
                                  variance = c(NA, NA, NA, NA),
                                  scale = c(1, 1, 1, 1))
  scale_factors <- get_rescale_df(lavaan_cont)
  expect_equal(scale_factors, scale_factors_ref, tolerance = 0.000001)


})
