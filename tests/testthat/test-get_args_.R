
test_that("get_args_weighted", {
  new_x <- c(bestviq2 = 22 ,
             bestnviq2 = 51.5 ,
             css2 =6,
             vdlst2 = 61.5)
  weights <- c(stand_y1 = 0.25,
               stand_y2 = 0.25,
               stand_y3 = 0.25,
               stand_y4 = 0.25)

  model_cont <-  readRDS("data/lavaan_cont.rds")
  model_cont <- process_lavaan_estimates(model_cont)
  prediction_matricies_cont <- get_args_weighted(new_x = new_x,
                                                weights = weights,
                                                processed_lavaan = model_cont)
  expect_equal(dim(prediction_matricies_cont$beta) , c(20, 1))
  expect_equal(dim(prediction_matricies_cont$x) , c(20, 1))
  expect_equal(dim(prediction_matricies_cont$weights) , c(20, 1))
  expect_equal(length(prediction_matricies_cont$sigma_e) , 1)
  expect_equal(dim(prediction_matricies_cont$sigma_beta) , c(20, 20))


  model_ord <- readRDS("data/lavaan_cont_and_ordinal.rds")
  model_ord <- process_lavaan_estimates(model_ord)
  prediction_matricies_cont <- get_args_weighted(new_x = new_x,
                                                 weights = weights,
                                                 processed_lavaan = model_ord)
  expect_equal(dim(prediction_matricies_cont$beta) , c(14, 1))
  expect_equal(dim(prediction_matricies_cont$x) , c(14, 1))
  expect_equal(dim(prediction_matricies_cont$weights) , c(14, 1))
  expect_equal(length(prediction_matricies_cont$sigma_e) , 1)
  expect_equal(dim(prediction_matricies_cont$sigma_beta) , c(14, 14))

  simple_model <- readRDS("data/simple_model.rds")
  model_simp<- process_lavaan_estimates(simple_model)
  prediction_matricies_simp <- get_args_weighted(new_x = new_x,
                                                weights = weights,
                                                processed_lavaan = model_simp)
  expect_equal(dim(prediction_matricies_simp$beta) , c(1, 1))
  expect_equal(dim(prediction_matricies_simp$x) , c(1, 1))
  expect_equal(dim(prediction_matricies_simp$weights) , c(1, 1))
  expect_equal(length(prediction_matricies_simp$sigma_e) , 1)
  expect_equal(dim(prediction_matricies_simp$sigma_beta) , c(1, 1))

})

test_that("get_args_single_outcome", {
  new_x <- c(bestviq2 = 22 ,
             bestnviq2 = 51.5 ,
             css2 =6,
             vdlst2 = 61.5)
  weights <- c(stand_y1 = 0.25,
               stand_y2 = 0.25,
               stand_y3 = 0.25,
               stand_y4 = 0.25)

  model_cont <-  readRDS("data/lavaan_cont.rds")
  model_cont <- process_lavaan_estimates(model_cont)
  args <- get_args_single_outcome(new_x = new_x,
                                  processed_lavaan = model_cont,
                                  outcome = "stand_y1")
  expect_equal(dim(args$beta) , c(5, 1))
  expect_equal(dim(args$x) , c(5, 1))
  expect_equal(length(args$weights) , 1)
  expect_equal(length(args$sigma_e) , 1)
  expect_equal(dim(args$sigma_beta) , c(5, 5))


  simple_model <- readRDS("data/simple_model.rds")

  coefs <- lavaan::parameterestimates(simple_model)
  beta <- coefs$est[1]
  se <- coefs$se[1]
  model_data <- lavaan::lavTech(simple_model, what = "data")
  preds <- model_data[[1]][,2]*beta
  mean_preds <- mean(preds)
  var_preds <- var(preds)

  scale_factors <- get_rescale_df(simple_model)

  model_simp<- process_lavaan_estimates(simple_model)
  args <- get_args_single_outcome(new_x = c(bestviq2 =22),
                                  processed_lavaan = model_simp,
                                  outcome = "y13")

  expect_equal(args$beta[1,1] , beta)
  expect_equal(args$x[1,1] , 22)
  expect_equal(args$sigma_e , scale_factors$scale^2)
  expect_equal(args$sigma_beta[1,1] , se^2)
  expect_equal(args$weights , scale_factors$scale)
  expect_equal(args$mean_adjustments , scale_factors$mean*scale_factors$scale)


})

test_that("weight_mean_adjustment", {
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
  processed_lavaan <- process_lavaan_estimates(model_cont)
  adjusted_weights <- adjust_weights(weights, processed_lavaan$rescale_df, beta = processed_lavaan$beta)
  mean_adj <- weight_mean_adjustment(adjusted_weights$w, processed_lavaan$rescale_df)
  expect_equal(mean_adj, 0)
})
