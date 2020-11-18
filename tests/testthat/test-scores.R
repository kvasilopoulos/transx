test_that("scores works", {
  x <- c(2,4,8,20)
  expect_error(score_z(x), NA)
  expect_error(score_t(x), NA)
  expect_error(score_mad(x), NA)
  expect_error(score_chisq(x), NA)
})

test_that("scores outliers", {
  x <- c(2,4,8,20)
  expect_equal(score_z(x), outliers::scores(x))
  expect_equal(score_t(x), outliers::scores(x, "t"))
  expect_equal(score_chisq(x), outliers::scores(x, "chisq"))
  expect_equal(score_mad(x), outliers::scores(x, "mad"))
})
