context("getOMLDataSet")

test_that("getOMLDataSet", {
  with_test_cache({
    data = getOMLDataSet(61)
    checkOMLDataSet(data)
    expect_data_frame(data$data, nrows = 150, ncols = 5, any.missing = FALSE)
    expect_data_frame(as.data.frame(data, nrows = 150, ncols = 5, any.missing = FALSE))
    expect_data_table(as.data.table(data, nrows = 150, ncols = 5, any.missing = FALSE))
    expect_equal(data$target.features, "class")
    # cache.only option
    data = getOMLDataSet(61, cache.only = TRUE)
    checkOMLDataSet(data)
 })
})
