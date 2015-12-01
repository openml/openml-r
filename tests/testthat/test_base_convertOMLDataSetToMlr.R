context("convertOMLDataSetToMlr")

test_that("convertOMLDataSetToMlr", {
  clearOMLCache()
  ds = getOMLDataSet(10)
  mlr.task = convertOMLDataSetToMlr(ds)
  
  expect_is(mlr.task, "Task")
  expect_equal(ds$desc$default.target.attribute, mlr.task$task.desc$target)
})
