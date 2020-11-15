
test_that("fill functionality", {
  vec <- c(1, NA, 2)
  expect_equal(lagx(vec, fill = mean), c(NA, 1, NA))
  expect_error(lagx(vec, fill = ~ mean(.x)), NA) # purrr-style functions
  expect_error(lagx(vec, fill = function(x) mean(x)), NA)
  expect_equal(lagx(vec, fill = ~ mean(.x, na.rm = TRUE)), c(1, 1, NA))
  expect_error(lagx(vec, fill = function(x) mean(x, na.rm = TRUE)), NA)
})


test_that("lag & leadx", {
  vec <- c(3, 5, NA, 4, 2)
  mean_na <- function(x) mean(x, na.rm = TRUE)

  expect_equal(lagx(vec), c(NA, 3, 5, NA, 4))
  expect_equal(lagx(vec, fill = 2), c(2, 3, 5, NA, 4))
  expect_equal(lagx(vec, fill = mean_na), c(4, 3, 5, NA, 4))

  expect_equal(leadx(vec), c(5, NA, 4, 2, NA))
  expect_equal(leadx(vec, fill = 2), c(5, NA, 4, 2, 2))
  # expect_equal(leadx(vec, fill = fill_locf))

})

