context("getOMLDataSet")

test_that("getOMLDataSet", {
  checkOMLDataSet = function(data) {
    expect_is(data, "OMLDataSet")
    expect_is(data$desc, "OMLDataSetDescription")
    expect_character(data$colnames.old, any.missing = FALSE)
    expect_equal(data$colnames.old, data$colnames.new)
  }
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
    dids = c(1479) #, 1484, 1566, 1468, 1514, 1515)
    for(i in dids) {
      data = getOMLDataSet(i)
      checkOMLDataSet(data)
      # cache only option
      expect_error(getOMLDataSet(i, cache.only = TRUE), "not found in cache with option")
    }
    expect_error(getOMLDataSet(17), "Data set has been deactivated.")
    expect_error(getOMLDataSet(473), "Data set is in preparation. You can download it as soon as it's active.")
  })
})
