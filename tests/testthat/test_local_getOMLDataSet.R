context("getOMLDataSet")

test_that("getOMLDataSet", {
  with_test_cache({
    data = getOMLDataSet(61)
    checkOMLDataSet(data)
    expect_data_frame(data$data, nrows = 150, ncols = 5, any.missing = FALSE)
    expect_equal(data$target.features, "class")
    # cache.only option
    data = getOMLDataSet(61, cache.only = TRUE)
    checkOMLDataSet(data)
 })
  with_empty_cache({
    data.ids = c(1479) #, 1484, 1566, 1468, 1514, 1515)
    for(data.id in data.ids) {
      # cache only option
      expect_error(getOMLDataSet(data.id, cache.only = TRUE), "not found in cache with option")
      data = getOMLDataSet(data.id)
      checkOMLDataSet(data)
    }
    expect_error(getOMLDataSet(17), "Data set has been deactivated.")
    expect_error(getOMLDataSet(473), "Data set is in preparation. You can download it as soon as it's active.")
  })
})
