context("runTaskMlr")

test_that("runTaskMlr", {
  with_test_cache({
    checkRun = function(res) {
      expect_is(res, "OMLMlrRun")
      expect_equal(length(res), 3L)
      expect_is(res$run$predictions, "data.frame")
      expect_is(res$bmr, "BenchmarkResult")
      expect_is(res$flow, "OMLFlow")
    }
    
    lrn = makeLearner("classif.rpart")
    task = getOMLTask(59)
    res = runTaskMlr(task, lrn)
    expect_true(res$run$task.id == 59)
    expect_subset(c("repeat", "fold", "row_id", "prediction", "truth"),
      colnames(reformatPredictions(res$bmr$results[[1]][[1]]$pred$data, task)))
    checkRun(res)
    # check if additional measure is computed
    res = runTaskMlr(task, lrn, measures = ber)
    expect_subset(c("ber"), colnames(getBMRPerformances(res$bmr)[[1]][[1]]))
    res = runTaskMlr(task, lrn, measures = list(ber, mmce))
    expect_subset(c("ber", "mmce"), colnames(getBMRPerformances(res$bmr)[[1]][[1]]))
    
    # results for splits must be the same
    res.again = runTaskMlr(task, lrn)
    expect_identical(res$bmr$results[[1]][[1]]$pred$data, res.again$bmr$results[[1]][[1]]$pred$data)
    
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
    
    ps.vals = lapply(res$run$parameter.setting, function(x) x$value)
    for (i in seq_along(ps.vals))
      expect_character(ps.vals[[i]], any.missing = FALSE)
  })
})
