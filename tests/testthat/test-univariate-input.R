
test_that("numeric works", {
  vec <- rnorm(10)
  expect_error(assert_univariate(vec), NA)
  vec_int <- as.integer(c(1,2,3,4))
  expect_error(assert_univariate(vec_int), NA)
})

test_that("ts works", {
  ts <- as.ts(rnorm(10))
  expect_error(assert_univariate(ts), NA)
  # Try increasing the dims here
  ts_mat <- as.ts(matrix(rnorm(4), ncol = 2))
  expect_error(assert_univariate(ts_mat))

})

test_that("chr does not works", {
  chr <- letters[1:10]
  expect_error(assert_univariate(chr))
})

test_that("list does not work", {
  lst1 <- list(ts1 = rnorm(4))
  expect_error(assert_univariate(lst1))
  lst2 <- list(ts1 = rnorm(4), ts2 = rnorm(4))
  expect_error(assert_univariate(lst2))
})

# 2D objects --------------------------------------------------------------

test_that("data.frame does not work", {
  df1 <- data.frame(ts1 = rnorm(4))
  expect_error(assert_univariate(df1))
  df2 <- data.frame(ts1 = rnorm(4), ts2 = rnorm(4))
  expect_error(assert_univariate(df2))
})


test_that("matrix does not work", {
  mat1 <- matrix(rnorm(4), ncol = 1)
  expect_error(assert_univariate(mat1))
  mat2 <- matrix(rnorm(4), ncol = 2)
  expect_error(assert_univariate(mat2))
})



