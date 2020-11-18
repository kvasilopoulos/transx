


test_that("winsorize", {
  set.seed(1234)
  x <- rnorm(10)
  x[1] <- x[1] * 10
  expect_equal(out_winsorise(x), DescTools::Winsorize(x))
})

test_that("threshold", {
  x <- c(-5, -1, 0, 1, 2, 10)
  expect_error(out_threshold(x), "should be specified")
  expect_equal(out_threshold(x, tlow = -3), c(NA, -1, 0, 1, 2, 10))
  expect_equal(out_threshold(x, thigh = 7), c(-5, -1, 0, 1, 2, NA))
  expect_equal(out_threshold(x, tlow = -3, thigh = 7), c(NA, -1, 0, 1, 2, NA))
})

test_that("pt", {
  x <- 1:10
  expect_equal(out_pt(x), c(NA, 2:9, NA))
  expect_equal(out_pt(x, .2, .8), c(NA, NA, 3:8, NA, NA))
})


test_that("iqr", {
  x <- 1:10
  expect_equal(out_iqr(c(0, 4, 5, 6, 10)), c(NA, 4, 5, 6, NA))
})



test_that("scores",{
  x <- c(-5, -1, 0, 1, 2, 10)
  expect_error(out_score_z(x), NA)
  expect_error(out_score_zrob(x), NA)
  expect_error(out_score_t(x), NA)
  expect_error(out_score_chisq(x), NA)
})
