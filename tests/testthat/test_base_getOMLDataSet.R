context("getOMLDataSet")

test_that("getOMLDataSet", {
  clearOMLCache()
  expect_error(getOMLDataSet(1L, cache.only = TRUE), "not found in cache")

  ds = getOMLDataSet(1L)
  expect_is(ds, "OMLDataSet")
  expect_true(ds$desc$id == 1L)

  expect_error(getOMLDataSet(1231109283), "Unknown dataset")
  
  # try different datasets from different task types
  tasks = tasks[with(tasks, NumberOfInstances < 1000 & NumberOfFeatures < 1000 &
                       (NumberOfSymbolicFeatures==0 | is.na(NumberOfSymbolicFeatures))), ]
  dids = split(tasks$did, tasks$task_type)
  dids = lapply(dids, function(X) tail(X, 3))
  
  for(i in unlist(dids)) expect_is(getOMLDataSet(i), "OMLDataSet")
})
