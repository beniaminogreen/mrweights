test_that("test is_dummy", {
  expect_true(is_dummy(c(1, 0, 0, 0, 1, 0)))
  expect_true(is_dummy(c(T, F, F, T, F, F)))

  expect_true(is_dummy(c(T, 1, 0, T, F, F)))
  expect_false(is_dummy(c(1, 2, 0, 0, 1, 0)))
  expect_false(is_dummy(iris$Species))
  expect_false(is_dummy(as.factor(iris$Species)))
})
