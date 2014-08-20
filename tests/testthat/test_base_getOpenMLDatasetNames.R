context("getOpenMLDatasetNames")

test_that("getOpenMLDatasetNames", {
  xs = getOpenMLDatasetNames()
  expect_true(is.character(xs))
  expect_true(length(xs) > 0L)
  expect_true(!any(is.na(xs)))
})

