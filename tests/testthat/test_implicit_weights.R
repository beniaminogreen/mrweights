test_that("implicit_weights throws an error if model has weights", {
  error_model <- lm(dist ~ speed, data = cars, weights = 1:50)
  expect_error(implicit_weights(error_model))
})
test_that("implicit_weights works properly", {
  test_model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species, data = iris)

  model1 <- lm(Sepal.Width ~ Petal.Length + Species, data = iris)
  model2 <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)

  model3 <- suppressWarnings(glm(
    Speciesversicolor ~ Petal.Length + Sepal.Width + Speciesvirginica,
    data = expand_model_frame(iris),
    family = "binomial"
  ))


  weights1 <- residuals(model1)^2
  weights1 <- weights1 / max(weights1)
  weights2 <- residuals(model2)^2
  weights2 <- weights2 / max(weights2)
  weights3 <- residuals(model3)^2
  weights3 <- weights3 / max(weights3)
  weights <- implicit_weights(test_model)


  expect_true(all(weights$Sepal.Width_weight == weights1))
  expect_true(all(weights$Petal.Length_weight == weights2))
  expect_true(all(weights$Speciesversicolor_weight == weights3))
})
