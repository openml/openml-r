context("getOpenMLDatasetNames")

test_that("getOpenMLDatasetNames", {
  xs = getOpenMLDatasetNames()
  expect_true(is.data.frame(xs))
  expect_true(nrow(xs) > 0L)
  expect_true(ncol(xs) == 3)
  expect_true(is.numeric(xs$did))
  expect_true(is.character(xs$name))
  expect_true(is.numeric(xs$version))
  expect_true(!any(is.na(xs)))
})

