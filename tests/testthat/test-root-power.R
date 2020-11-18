test_that("root", {
  expect_equal(root(4, 1), 4)
  expect_equal(root(4, 2), 2)
  x <- rnorm(3, 10)
  expect_equal(root_sq(x), sqrt(x))
  expect_equal(root_cubic(8, 3), 2)

  expect_equal(root(-4, 2, TRUE), -2)
})

test_that("pow", {
  expect_equal(pow(4, 1), 4)
  expect_equal(pow(2, 2), 4)
  x <- rnorm(3, 10)
  expect_equal(pow(-4, 2, TRUE), -16)
})

test_that("pow transx", {
  x <- rnorm(10)
  expect_error(pow_boxcox(x), NA)
  expect_error(pow_yj(x), NA)
  expect_error(pow_tukey(x), NA)
  expect_error(pow_manly(x), NA)
})
