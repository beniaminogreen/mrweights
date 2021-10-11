test_that("implicit_weights throws an error if model has weights", {
  error_model <- lm(dist ~ speed, data = cars, weights = 1:50)
  expect_error(implicit_weights(error_model))
})
