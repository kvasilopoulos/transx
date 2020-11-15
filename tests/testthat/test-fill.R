test_that("fill as numeric", {
  x <- 1:5
  expect_error(lagx(x, fill = 1), NA)
  expect_error(lagx(x, 2, fill = 1), NA)
  expect_error(lagx(x, 3, fill = c(1,2)), "replacement length")
})


test_that("fill as function", {
  x <- 1:5
  expect_error(lagx(x, fill = mean), NA)
  expect_error(lagx(x, 2, fill = function(x) mean(x)), NA)
  expect_error(lagx(x, 2, fill = ~ mean(.x)), NA)

  expect_error(lagx(x, 1, fill = fill_nocb), NA)
  expect_error(lagx(x, 1, fill = ~ fill_locf(.x, .y, fail = 100)), NA)
})


test_that("fill_*", {
  x <- 1:5
  expect_error(lagx(x, fill = fill_nocb), NA)
  expect_error(lagx(x, fill = fill_locf), NA)
  expect_error(lagx(x, fill = fill_linear), NA)
  expect_error(lagx(x, fill = fill_spline), NA)
})
