context("toMlr")

test_that("toMlr", {
  dsd = downloadOpenMLData("iris")
  task = toMlr(dsd)
  expect_is(task, "ClassifTask")
  expect_true(task$task.desc$target == "class")
  expect_true(task$task.desc$type == "classif")

  task = toMlr(dsd, target = "sepallength")
  expect_true(task$task.desc$target == "sepallength")
  expect_true(task$task.desc$type == "regr")

  oml.task = downloadOpenMLTask(1)
  mlr.task = toMlr(oml.task)
  expect_true(mlr.task$mlr.task$task.desc$target == "class")
  expect_is(mlr.task$mlr.rin, "ResampleInstance")
  expect_is(mlr.task$mlr.measures, "list")
  expect_true(setequal(mlr.task$orig.lvls, c("1", "2", "3", "4", "5", "U")))

  oml.task$type = "Clustering"
  expect_error(toMlr(oml.task))
})
