test_that("display", {
  expect_message(lagx(c(1,2,3)), "Filling")
  options(transx.display = FALSE)
  expect_message(lagx(c(1,2,3)), NA)
  options(transx.display = TRUE)
  expect_message(lagx(c(1,2,3)), "Filling")
})

test_that("na.rm" ,{
  expect_message(demean(c(NA,2,3)), "is set to TRUE")
  options(transx.na.rm = FALSE)
  expect_equal(demean(c(NA,2,3)), c(NA_real_, NA_real_, NA_real_))
  expect_message(demean(c(NA,2,3)), NA)
  options(transx.na.rm = TRUE)
  expect_message(demean(c(NA,2,3)), "is set to TRUE")
})

test_that("na.rm & display" ,{
  expect_message(demean(c(NA,2,3)), "is set to TRUE")
  options(transx.display = FALSE)
  expect_equal(demean(c(NA,2,3)), c(-0.5, 0.5))
  expect_message(demean(c(NA,2,3)), NA)
  options(transx.display = TRUE)
  expect_message(demean(c(NA,2,3)), "is set to TRUE")
})
