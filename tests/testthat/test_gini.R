test_that("Calculation for Gini Coefficient Works Correctly", {
  perfect_inequality <- c(rep(0, 100), 100)
  perfect_equality <- c(rep(1, 100))

  expect_equal(gini(perfect_inequality), .99, tolerance = 1e-2)
  expect_equal(gini(perfect_equality), 0, tolerance = 1e-2)
  expect_equal(gini(1:100), .330, tolerance = 1e-2)
  expect_equal(gini((1:100)^2), .5, tolerance = 1e-2)
})
