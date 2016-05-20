context("convertMlrTaskToOMLDataSet")

test_that("convertMlrTaskToOMLDataSet", {
  oml.iris = convertMlrTaskToOMLDataSet(iris.task)
  expect_equal(oml.iris$colnames.old, oml.iris$colnames.new)
  expect_equal(oml.iris$colnames.old, colnames(oml.iris$data))
  expect_equal(oml.iris$target.features, getTaskTargetNames(iris.task))
  expect_equal(oml.iris$data, getTaskData(iris.task))
  
  oml.iris = convertMlrTaskToOMLDataSet(iris.task, description = "this is iris")
  expect_equal(oml.iris$desc$description, "this is iris")
  expect_equal(oml.iris$colnames.old, oml.iris$colnames.new)
  expect_equal(oml.iris$colnames.old, colnames(oml.iris$data))
  expect_equal(oml.iris$target.features, getTaskTargetNames(iris.task))
  expect_equal(oml.iris$data, getTaskData(iris.task))
})
