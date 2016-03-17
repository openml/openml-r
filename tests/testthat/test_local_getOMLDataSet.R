context("getOMLDataSet/local")

test_that("getOMLDataSet", {
  with_test_cache({
    data = getOMLDataSet(61)
    expect_is(data, "OMLDataSet")
    expect_is(data$desc, "OMLDataSetDescription")
    expect_data_frame(data$data, nrows = 150, ncols = 5, any.missing = FALSE)
    expect_character(data$colnames.old, any.missing = FALSE)
    expect_equal(data$colnames.old, data$colnames.new)
    expect_equal(data$target.features, "class")
 })
})
