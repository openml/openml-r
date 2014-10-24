context("toMlr")

test_that("toMlr", {
  dsd = downloadOMLDataSet(id = 10, session.hash)
  task = toMlr(dsd)
  expect_is(task, "ClassifTask")
  expect_true(task$task.desc$target == "class")
  expect_true(task$task.desc$type == "classif")

  target = names(dsd$data[lapply(dsd$data, is.numeric) == TRUE])[1]
  task = toMlr(dsd, target = target)
  expect_true(task$task.desc$target == target)
  expect_true(task$task.desc$type == "regr")

  oml.task = downloadOMLTask(1, session.hash)
  mlr.task = toMlr(oml.task)
  expect_true(mlr.task$mlr.task$task.desc$target == "class")
  expect_is(mlr.task$mlr.rin, "ResampleInstance")
  expect_is(mlr.task$mlr.measures, "list")
  expect_true(setequal(mlr.task$orig.lvls, c("1", "2", "3", "4", "5", "U")))

  oml.task$type = "Clustering"
  expect_error(toMlr(oml.task))
})
