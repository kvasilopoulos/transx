
test_that("plot", {
  x <- rnorm(10)
  x2 <- cbind(x, x)
  expect_error(plotx(x), NA)
  expect_error(plotx(x2), NA)

  #...
  expect_error(plotx(x, main ="rnorm"), NA)
  expect_error(plotx(x2, main ="rnorm"), NA)

  expect_error(plotx(x, xlab ="nx"), "xlab is not available")
  expect_error(plotx(x2, xlab ="nx"), "xlab is not available")
  expect_error(plotx(x, ylab ="nx"), "ylab is not available")
  expect_error(plotx(x2, ylab ="nx"), "ylab is not available")
})
