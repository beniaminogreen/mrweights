test_that("expand_model_frame works correctly", {
  testcase_1 <- expand_model_frame(iris)
  expected_names <- c(
    "Sepal.Length",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width",
    "Speciesversicolor",
    "Speciesvirginica"
  )

  # the names should look like the ones above
  expect_true(all(names(testcase_1) == expected_names))

  # All columns should be numeric
  expect_true(all(sapply(testcase_1, is.numeric)))


  expect_equal(
    as.numeric(iris$Species == "versicolor"),
    testcase_1$Speciesversicolor
  )
  expect_equal(
    as.numeric(iris$Species == "virginica"),
    testcase_1$Speciesvirginica
  )

  # Should do nothing to a dataframe which is already all numeric
  expect_equal(expand_model_frame(cars), cars)
})
