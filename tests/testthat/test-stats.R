test_that("gmean", {
  x <- c(1,2,3,4,5)
  expect_equal(gmean(x), 2.605, tol = 0.01)
})

test_that("mode", {
  x <- c(0,0,0,1, 1,1,1,2)
  expect_equal(modex(x), 0.9602, tol = 0.01)
  expect_equal(modex_int(x), 1)
})

test_that("moments",{
  set.seed(124)
  x <- rnorm(1000)
  expect_equal(skewness(x), 0, tol =0.1)
  expect_equal(kurtosis(x), 3, tol = 0.1)
})
