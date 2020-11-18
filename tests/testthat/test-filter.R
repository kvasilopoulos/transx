

test_that("hamilton", {
  expect_error(filter_hamilton(unemp), NA)
})

test_that("filter works", {
  expect_equal(filter_hp(unemp), mFilter::hpfilter(unemp)$cycle)
  expect_equal(select_lambda(), 1600)
  expect_equal(select_lambda("annual", type = "ru2002"), 6.25)

  expect_equal(filter_cf(unemp), mFilter::cffilter(unemp)$cycle[,1])
  expect_equal(filter_bw(unemp), mFilter::bwfilter(unemp)$cycle)
  expect_equal(filter_tr(unemp), mFilter::trfilter(unemp)$cycle[,1])
  expect_equal(filter_bk(unemp), mFilter::bkfilter(unemp)$cycle[,1])
})


test_that("bk fill", {
  bk <- mFilter::bkfilter(unemp)$cycle[,1]
  bk[is.na(bk)] <- 0
  expect_equal(filter_bk(unemp, fill = 0), bk)
})
