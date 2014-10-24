context("downloadOMLDataSet")

test_that("downloadOMLDataSet", {
  ds = downloadOMLDataSet(1)
  expect_is(ds, "OMLDataSet")
  expect_true(ds$desc$id == 1)

  expect_error(downloadOMLDataSet(1231109283, session.hash), "Unknown dataset")
})
