

test_that("lag & leadx", {
  vec <- c(3, 5, NA, 4, 2)
  mean_na <- function(x) mean(x, na.rm = TRUE)

  expect_equal(lagx(vec), c(NA, 3, 5, NA, 4))
  expect_equal(lagx(vec, fill = 2), c(2, 3, 5, NA, 4))
  expect_equal(lagx(vec, fill = mean_na), c(4, 3, 5, NA, 4))

  expect_equal(leadx(vec), c(5, NA, 4, 2, NA))
  expect_equal(leadx(vec, fill = 2), c(5, NA, 4, 2, 2))
})


test_that("diffx", {
  x <- c(2,4,8,20)
  expect_error(diffx(x), NA)
  expect_error(rdiffx(x), NA)
  expect_error(ldiffx(x), NA)
})

test_that("demean", {
  x <- c(2,4,8,20)
  expect_error(demean(x), NA)
  expect_error(demedian(x), NA)
  expect_error(demode(x), NA)
})


test_that("demean", {
  x <- c(2,4,8,20)
  expect_error(rebase(x, 2), NA)
  expect_error(rebase_origin(x), NA)
})


test_that("range", {
  x <- c(2,4,8,20)
  expect_error(scale_range(x, c(1,10)), NA)
  expect_error(scale_minmax(x), NA)
  expect_error(scale_unit_len(x), NA)
})

test_that("range", {
  x <- c(2,4,8,20)
  expect_error(std_mean(x), NA)
  expect_error(std_median(x), NA)
})
