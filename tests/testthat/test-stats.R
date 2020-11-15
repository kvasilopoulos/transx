test_that("gmean", {
  x <- c(1,2,3,4,5)
  expect_equal(gmean(x), 2.605, tol = 0.01)
})
