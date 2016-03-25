context("listOMLDataSets")

skip_on_cran()

test_that("listOMLDataSets", {
  for (dsl in list(.listOMLDataSets(), .listOMLDataSets(tag = "test"))) {
    expect_data_frame(dsl, col.names = "unique", min.rows = 1)
    expect_set_equal(names(dsl), c("did", "status", "name", "MajorityClassSize",
      "MaxNominalAttDistinctValues", "MinorityClassSize", "NumBinaryAtts",
      "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
      "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
      "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"))
    expect_integer(dsl$did, any.missing = FALSE, unique = TRUE)
    expect_factor(dsl$status, any.missing = FALSE)
    expect_character(dsl$name, any.missing = FALSE)
  }
})
