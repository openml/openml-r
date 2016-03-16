context(".listOMLDataSets")

test_that(".listOMLDataSets", {
  dsl = .listOMLDataSets()
  expect_is(dsl, "data.frame")
  expect_true(nrow(dsl) > 100L && ncol(dsl) == 14L)
  expect_true(setequal(names(dsl), c("did", "status", "name", "MajorityClassSize",
    "MaxNominalAttDistinctValues", "MinorityClassSize", "NumBinaryAtts",
    "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
    "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
    "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")))
  inds = which(names(dsl) %in% c("status", "name"))
  expect_true(all(apply(dsl[, inds], 2, is.character)))

  # check search for tagged datasets
  dsl = .listOMLDataSets(tag = "test")
  expect_is(dsl, "data.frame")
  expect_true(nrow(dsl) > 0 &&  ncol(dsl) == 14L)
})
