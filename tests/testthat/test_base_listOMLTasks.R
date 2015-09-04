context("listOMLTasks")

test_that("listOMLTasks", {
  exp.names = c("task_id", "task_type", "did", "status", "name", "tags",
    "estimation_procedure", "evaluation_measures", "NumberOfClasses",
    "NumberOfFeatures", "NumberOfInstances", "NumberOfInstancesWithMissingValues",
    "NumberOfMissingValues", "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"
  )

  tasks = listOMLTasks()
  expect_is(tasks, "data.frame")
  expect_true(nrow(tasks) > 5L)
  expect_true(isSuperset(colnames(tasks), exp.names))
})
