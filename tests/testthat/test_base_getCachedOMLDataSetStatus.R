context("getCachedOMLDataSetStatus")

test_that("getCachedOMLDataSetStatus", {
  clearOMLCache()

  dids = 1:2
  populateOMLCache(dids = dids)

  status = getCachedOMLDataSetStatus()
  expect_is(status, "data.frame")
  expect_equal(nrow(status), length(dids))
  expect_true(all(dids %in% status$did))

  clearOMLCache()
})
