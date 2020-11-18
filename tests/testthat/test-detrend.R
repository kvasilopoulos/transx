test_that("multiplication works", {
  t <- 1:20
  x <-  3*sin(t) + t
  expect_error(dtrend_lin(x), NA)
  expect_error(dtrend_quad(x), NA)
  expect_error(dtrend_poly(x, 3), NA)

  expect_error(dtrend_poly(x, 3, bp = 5), NA)
})
