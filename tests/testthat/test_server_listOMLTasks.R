context("listOMLTasks")

skip_on_cran()

test_that("listOMLTasks", {
  exp.names = c("task.id", "task.type", "data.id", "status", "format", "name", "target.feature", "tags",
    "estimation.procedure", "evaluation.measures", "majority.class.size",
    "max.nominal.att.distinct.values", "minority.class.size", #"num.binary.atts",
    "number.of.classes", "number.of.features", "number.of.instances",
    "number.of.instances.with.missing.values", "number.of.missing.values",
    "number.of.numeric.features", "number.of.symbolic.features"
  )

  tasks = .listOMLTasks(limit = 10L)
  expect_data_frame(tasks, nrows = 10L, col.names = "unique")
  #expect_set_equal(exp.names, names(tasks))

  # check number of classes
  tasks = as.data.table(tasks)

  expect_true(all(tasks[number.of.classes == 2, list(ok = (minority.class.size + majority.class.size) == number.of.instances)]$ok))
  expect_true(all(tasks[task.type == "Supervised Classification", list(ok = (minority.class.size + majority.class.size) <= number.of.instances)]$ok, na.rm = TRUE))
  #expect_true(all(tasks[, list(ok = NumBinaryAtts <= NumberOfSymbolicFeatures)]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (number.of.numeric.features + number.of.symbolic.features) <= number.of.features)]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (number.of.instances.with.missing.values <= number.of.instances))]$ok, na.rm = TRUE))
  expect_true(all(tasks[, list(ok = (number.of.missing.values <= as.numeric(number.of.instances) * as.numeric(number.of.features)))]$ok, na.rm = TRUE))

  tasks1 = .listOMLTasks(tag = "study_1")
  expect_data_frame(tasks1, min.rows = 10L, col.names = "unique")
  #expect_set_equal(exp.names, names(tasks1))

  # check if listing one tag works
  one.task = .listOMLTasks(limit = 1)
  expect_data_frame(one.task, nrows = 1, col.names = "unique")
  #expect_set_equal(exp.names, names(one.task))

  # check if evaluation.measures and estimation.procedures are NA for Subgroup Discovery task type
  exp.tag = "Cortana"
  tasks = .listOMLTasks(tag = exp.tag)
  expect_true(unique(tasks$task.type) == "Subgroup Discovery")
  expect_true(sum(is.na(tasks$evaluation.measures)) == nrow(tasks))
  expect_true(all(grepl(exp.tag, tasks$tags)))
  #expect_set_equal(exp.names, names(tasks))

  # check if status works
  tasks = .listOMLTasks(status = "in_preparation", limit = 10)
  expect_string(unique(tasks$status))
  expect_true(unique(tasks$status) == "in_preparation")

  tasks = .listOMLTasks(status = "deactivated", limit = 10)
  expect_data_frame(tasks, nrows = 10L, col.names = "unique")
  expect_string(unique(tasks$status))
  expect_true(unique(tasks$status) == "deactivated")
})

test_that("listOMLTasks works with data.name filter", {
  exp.name = "iris"
  tasks = .listOMLTasks(limit = 10L, data.name = exp.name)
  expect_data_frame(tasks)
  expect_true(all(tasks$name == exp.name))
})
