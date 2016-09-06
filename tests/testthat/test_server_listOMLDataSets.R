context("listOMLDataSets")

skip_on_cran()

test_that("listOMLDataSets", {
  for (dsl in list(.listOMLDataSets(limit = 10L), .listOMLDataSets(tag = "study_1", limit = 10L))) {
    expect_data_frame(dsl, col.names = "unique", min.rows = 1)
    expect_set_equal(names(dsl), c("data.id", "status", "format", "name", "MajorityClassSize",
      "MaxNominalAttDistinctValues", "MinorityClassSize", #"NumBinaryAtts",
      "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
      "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
      "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"))
    expect_integer(dsl$data.id, any.missing = FALSE, unique = TRUE)
    expect_factor(dsl$status, any.missing = FALSE)
    expect_character(dsl$name, any.missing = FALSE)
  }
  ds = .listOMLDataSets(NumberOfInstances = c(40, 100), NumberOfFeatures = c(5, 10),
    NumberOfClasses = 2, NumberOfMissingValues = 0)
  expect_true(min(ds$NumberOfInstances) >= 40)
  expect_true(max(ds$NumberOfInstances) <= 100)
  expect_true(min(ds$NumberOfFeatures) >= 5)
  expect_true(max(ds$NumberOfFeatures) <= 10)
  expect_true(unique(ds$NumberOfClasses) == 2)
  expect_true(unique(ds$NumberOfMissingValues) == 0)
})
