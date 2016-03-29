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
 })
  with_empty_cache({
    dids = c(1479) #, 1484, 1566, 1468, 1514, 1515)
    for(i in dids) {
      data = getOMLDataSet(i)
      checkOMLDataSet(data)
    }
  })
})
