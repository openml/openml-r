context("runTaskMlr")

test_that("runTaskMlr", {
  checkRun = function(res) {
    expect_is(res, "list")
    expect_equal(length(res), 3L)
    expect_is(res$run$predictions, "data.frame")
    expect_is(res$bmr, "BenchmarkResult")
    expect_is(res$flow, "OMLFlow")
  }

  lrn = makeLearner("classif.rpart")
  task = getOMLTask(1L)
  res = runTaskMlr(task, lrn)
  expect_true(res$run$task.id == 1L)
  checkRun(res)

  task = getOMLTask(9991L)
  res = runTaskMlr(task, lrn)
  expect_true(res$run$task.id == 9991L)
  checkRun(res)

  # results for splits must be the same
  res.again = runTaskMlr(task, lrn)
  expect_identical(res$bmr$results[[1]][[1]]$pred$data,
    res.again$bmr$results[[1]][[1]]$pred$data)

  # check converting OML measures to Mlr measures
  task$input$evaluation.measures = "predictive_accuracy"
  expect_true(convertOMLMeasuresToMlr(task$input$evaluation.measures)[[1]]$id == "acc")
  task$input$evaluation.measures = "area_under_roc_curve"
  expect_true(convertOMLMeasuresToMlr(task$input$evaluation.measures)[[1]]$id == "auc")

  # check converting datasets to mlr Tasks
  mlr.task = convertOMLDataSetToMlr(task$input$data.set, task$task.type)
  expect_is(mlr.task, "Task")

  # check if data splits are converted properly
  rin = convertOMLSplitsToMlr(task$input$estimation.procedure, mlr.task)
  expect_is(rin, "ResampleInstance")
  ds = task$input$estimation.procedure$data.splits
  splits = lapply(split(ds, ds$type), function(X) split(X$rowid, X$fold))
  expect_identical(rin$train.inds, unname(splits[[1]]))
  expect_identical(rin$test.inds, unname(splits[[2]]))

  expect_error(runTaskMlr(task, makeLearner("regr.rpart")), "regr")

  # check if all parameter values are strings
  lrn = makeLearner("classif.glmboost", family = mboost::Binomial())
  task = getOMLTask(37)
  res = runTaskMlr(task, lrn)
  ps.vals = lapply(res$run$parameter.setting, function(x) x$value)
  for (i in seq_along(ps.vals))
    expect_character(ps.vals[[i]])

  # FIXME: disabled for now. bad test also.
  # a test should be with a learner that breaks for UNFORSEEABLE reasons on data
  # task$input$data.set$data[1, 3] = NA
  # run = runTaskMlr(task, makeLearner("classif.randomForest"))
  # expect_true(is.null(run$predictions) && testString(run$error.message))

  # check for error message / failed runs
  #task = getOMLTask(261)
  lrn = makeLearner("classif.randomForest")
  lrn$properties = c(lrn$properties, c("missings", "factors"))
  # should not produce an error message
  res = runTaskMlr(task, lrn)
  expect_true(is.na(res$run$error.message))
  # introduce NAs: should produce an error message
  task$input$data.set$data[1:2, 1] = NA
  configureMlr(on.learner.error = "quiet")
  res = runTaskMlr(task, lrn)
  configureMlr(on.learner.error = "stop")
  expect_false(is.na(res$run$error.message))

  # local sanity check (account needs read-write permissions)
  if (!identical(Sys.getenv("TRAVIS"), "true")) {
  # upload the run that contains the error message
  run.id = uploadOMLRun(res$run)
  # download it again and check for error message
  run.down = getOMLRun(run.id)
  # FIXME: serverapi does not return error.message field although it is in the xml
  #expect_false(is.na(run.down$error.message))
  run.list = listOMLRuns(run.id = run.id)
  expect_equal(as.character(run.list$error.message),
    gsub("\n$", "", res$run$error.message))
  deleteOMLObject(id = run.id, object = "run")
  }
})
