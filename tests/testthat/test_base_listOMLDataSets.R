context("listOMLDataSets")

test_that("listOMLDataSets", {
  dsl = listOMLDataSets(session.hash)
  expect_is(dsl, "data.frame")
  expect_true(nrow(dsl) > 100L && ncol(dsl) == 8L)
  expect_true(setequal(names(dsl), c("did", "status", "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances", "NumberOfInstancesWithMissingValues", "NumberOfMissingValues", "NumberOfNumericFeatures")))
})
