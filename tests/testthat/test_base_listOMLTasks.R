context("listOMLTasks")

test_that("listOMLTasks", {
  exp.names = c("task.id", "task.type", "did", "status", "name", "target.feature", "tags",
    "estimation.procedure", "evaluation.measures", "MajorityClassSize",
    "MaxNominalAttDistinctValues", "MinorityClassSize", "NumBinaryAtts", 
    "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances", 
    "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
    "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"
  )

  tasks = listOMLTasks()
  expect_is(tasks, "data.frame")
  expect_true(nrow(tasks) > 5L)
  expect_true(isSuperset(colnames(tasks), exp.names))
  
  # check if qualities are meaningful
  # FIXME: code below must also work with na.rm = FALSE
  na.rm = TRUE
  
  # check number of classes
  tasks2 = subset(tasks, NumberOfClasses == 2)
  expect_true(all(rowSums(tasks2[, c("MinorityClassSize", "MajorityClassSize")]) == tasks2$NumberOfInstances))
  
  tasksClass = subset(tasks, task.type == "Supervised Classification")
  sumMinMajClass = rowSums(tasksClass[, c("MinorityClassSize", "MajorityClassSize")])
  expect_true(all(sumMinMajClass <= tasksClass$NumberOfInstances, na.rm = na.rm))
  
  # check features
  expect_true(all(tasks$NumBinaryAtts <= tasks$NumberOfSymbolicFeatures, na.rm = na.rm))
  
  sumNumSymFeat = rowSums(tasks[, c("NumberOfNumericFeatures", "NumberOfSymbolicFeatures")])
  expect_true(all(tasks$NumberOfFeatures >= sumNumSymFeat, na.rm = na.rm))
  
  # FIXME: sometimes NumFeat + SymFeat = AllFeat and sometimes NumFeat + SymFeat + 1 = AllFeat 
#   sumNumSymFeat = rowSums(tasks[, c("NumberOfNumericFeatures", "NumberOfSymbolicFeatures")]) + 1
#   expect_true(all(tasks$NumberOfFeatures == sumNumSymFeat))
#   cols = c("task.id", "NumberOfNumericFeatures", "NumberOfSymbolicFeatures", "NumberOfFeatures", "task.type")
#   summary(tasks[(tasks$NumberOfFeatures != sumNumSymFeat), cols])
#   summary(tasks[(tasks$NumberOfFeatures == sumNumSymFeat), cols])
  
  # check missings
  expect_true(all(tasks$NumberOfInstancesWithMissingValues <= tasks$NumberOfInstances, na.rm = na.rm))
  expect_true(all(tasks$NumberOfMissingValues <= as.numeric(tasks$NumberOfInstances)*as.numeric(tasks$NumberOfFeatures), na.rm = na.rm))
})
