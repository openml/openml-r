context("listOMLTasks")

skip_on_cran()

test_that("listOMLTasks", {
  exp.names = c("task.id", "task.type", "did", "status", "name", "target.feature", "tags",
    "estimation.procedure", "evaluation.measures", "MajorityClassSize",
    "MaxNominalAttDistinctValues", "MinorityClassSize", "NumBinaryAtts",
    "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
    "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
    "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"
  )

  tasks = listOMLTasks()
  expect_data_frame(tasks, min.rows = 6L, col.names = "unique")
  expect_set_equal(exp.names, names(tasks))

  # check number of classes
  tasks = as.data.table(tasks)

  expect_true(all(tasks[NumberOfClasses == 2, list(ok = (MinorityClassSize + MajorityClassSize) == NumberOfInstances)]$ok))
  expect_true(all(tasks[task.type == "Supervised Classification", list(ok = (MinorityClassSize + MajorityClassSize) <= NumberOfInstances)]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = NumBinaryAtts <= NumberOfSymbolicFeatures)]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (NumberOfNumericFeatures + NumberOfSymbolicFeatures) <= NumberOfFeatures)]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (NumberOfInstancesWithMissingValues <= NumberOfInstances))]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (NumberOfMissingValues <= as.numeric(NumberOfInstances) * as.numeric(NumberOfFeatures)))]$ok, na.rm = TRUE))
})
