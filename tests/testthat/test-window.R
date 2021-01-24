
test_that("window names",{
  x <- 1:10
  expect_equal(names(blck_data(x)), NULL)
  expect_equal(names(rec_data(x)), NULL)
  expect_equal(names(roll_data(x)), NULL)
})


test_that("roll", {
  x <- rnorm(100)
  expect_equal(RcppRoll::roll_max(x), transx::roll_max(x))
  expect_equal(RcppRoll::roll_max(x, by = 4, fill = NA), transx::roll_max(x, n = 4))
})

test_that("roll keeps same length",{
  x <- rnorm(100)
  expect_length(roll_data(x), 100)
  expect_length(roll_data(x, n = 2), 99)
  expect_length(roll_data(x, step = 2), 50)

  roll_fn(x, mean, n = 2)

})


test_that("rec", {
  x <- rnorm(100)
  expect_equal(cumstats::cummean(x), transx::rec_mean(x))
  expect_equal(cumstats::cumvar(x),  transx::rec_var(x))
  expect_equal(cumstats::cumkurt(x), transx::rec_fn(x, kurtosis))
  expect_equal(cumstats::cummode(x), transx::rec_fn(x, modex))
})

test_that("blck",{
  x <- 1:4
  expect_equal(blck_data(x), list(c(1,2), c(3,4)))
})
