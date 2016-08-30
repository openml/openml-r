context("listOMLTasks")

skip_on_cran()

test_that("listOMLTasks", {
  exp.names = c("task.id", "task.type", "did", "status", "format", "name", "target.feature", "tags",
    "estimation.procedure", "evaluation.measures", "MajorityClassSize",
    "MaxNominalAttDistinctValues", "MinorityClassSize", #"NumBinaryAtts",
    "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
    "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
    "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"
  )

  tasks = .listOMLTasks(limit = 10L)
  expect_data_frame(tasks, nrows = 10L, col.names = "unique")
  expect_set_equal(exp.names, names(tasks))

  # check number of classes
  tasks = as.data.table(tasks)

  expect_true(all(tasks[NumberOfClasses == 2, list(ok = (MinorityClassSize + MajorityClassSize) == NumberOfInstances)]$ok))
  expect_true(all(tasks[task.type == "Supervised Classification", list(ok = (MinorityClassSize + MajorityClassSize) <= NumberOfInstances)]$ok, na.rm = TRUE))
  #expect_true(all(tasks[, list(ok = NumBinaryAtts <= NumberOfSymbolicFeatures)]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (NumberOfNumericFeatures + NumberOfSymbolicFeatures) <= NumberOfFeatures)]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (NumberOfInstancesWithMissingValues <= NumberOfInstances))]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (NumberOfMissingValues <= as.numeric(NumberOfInstances) * as.numeric(NumberOfFeatures)))]$ok, na.rm = TRUE))
  
  tasks1 = .listOMLTasks(tag = "study_1")
  expect_data_frame(tasks1, min.rows = 10L, col.names = "unique")
  expect_set_equal(exp.names, names(tasks1))
  
  # check if listing one tag works
  one.task = .listOMLTasks(limit = 1)
  expect_data_frame(one.task, nrows = 1, col.names = "unique")
  expect_set_equal(exp.names, names(one.task))
  
  # check if evaluation.measures and estimation.procedures are NA for Subgroup Discovery task type
  tasks = .listOMLTasks(tag = "Cortana")
  expect_true(unique(tasks$task.type) == "Subgroup Discovery")
  expect_true(sum(is.na(tasks$evaluation.measures)) == nrow(tasks))
  expect_set_equal(exp.names, names(tasks))
})
