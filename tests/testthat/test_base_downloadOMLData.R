context("downloadOMLDataSet")

test_that("downloadOMLDataSet", {
  ds = downloadOMLDataSet(1)
  expect_is(ds, "OMLDataSet")
  expect_true(ds$desc$id == 1)

  expect_error(downloadOMLDataSet(12311092831203), "Unknown dataset")
})
