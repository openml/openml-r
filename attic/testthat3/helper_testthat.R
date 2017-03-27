checkOMLDataSet = function(data) {
  expect_is(data, "OMLDataSet")
  expect_is(data$desc, "OMLDataSetDescription")
  expect_character(data$colnames.old, any.missing = FALSE)
  expect_equal(data$colnames.old, data$colnames.new)
}
