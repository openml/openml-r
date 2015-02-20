context("listOMLDataSets")

test_that("listOMLDataSets", {
  dsl = listOMLDataSets(session.hash)
  expect_is(dsl, "data.frame")
  expect_true(nrow(dsl) > 100L && ncol(dsl) == 10L)
  expect_true(setequal(names(dsl), c("did", "status", "name", "NumberOfClasses", "NumberOfFeatures",
    "NumberOfInstances", "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
    "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")))
  inds = which(names(dsl) %nin% c("status", "name"))
  expect_true(all(apply(dsl[, inds], 2, is.integer)))
})
