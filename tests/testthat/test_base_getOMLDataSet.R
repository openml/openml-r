context("getOMLDataSet")

test_that("getOMLDataSet", {
  ds = getOMLDataSet(1L, session.hash = session.hash)
  expect_is(ds, "OMLDataSet")
  expect_true(ds$desc$id == 1L)

  expect_error(getOMLDataSet(1231109283, session.hash = session.hash), "Unknown dataset")
})
