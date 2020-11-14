# body <- x[-idx]
# out <- fill_(body, idx, fill, fill_fun)

# body <- body_(x, idx)
# out <- fill_(body, idx, fill, fill_fun)


test_that("body_ and fill_", {
  vec <- c(1,3,5,6,2)
  idx <- 4
  body <- body_(vec, idx)
  expect_equal(fill_(body, idx, NA, NULL), c(1,3,5,NA,2))
})
