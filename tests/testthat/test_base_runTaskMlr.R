context("runTaskMlr")

test_that("runTaskMlr", {
  task = getOMLTask(1)
  lrn = makeLearner("classif.rpart")
  run = runTaskMlr(task, lrn)

  expect_is(run, "OMLMlrRun")
  expect_is(run$predictions, "data.frame")
  expect_true(run$task.id == 1L)

  expect_error(runTaskMlr(task, makeLearner("surv.coxph")), "does not correspond")

  task$input$data.set$data[1, 3] = NA
  run = runTaskMlr(task, makeLearner("classif.randomForest"))
  expect_true(is.null(run$predictions) && testString(run$error.message))
})
