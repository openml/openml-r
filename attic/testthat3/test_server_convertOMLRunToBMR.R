context("convertOMLRunToBMR")

test_that("convertOMLRunToBMR", {
  checkBMR = function(bmr) {
    expect_is(bmr, "BenchmarkResult")
    res.classes = c("character", "character", "TaskDesc", "data.frame", "data.frame", "numeric",
      "Prediction", "list", "data.frame", "list", "numeric", "Learner" )
    for (j in seq_along(res.classes)) expect_is(bmr$results[[1]][[1]][[j]], res.classes[j])
    expect_equal(dim(bmr$results[[1]][[1]]$measures.train), dim(bmr$results[[1]][[1]]$measures.test))
    for (j in seq_along(bmr$measures)) {
      expect_is(bmr$measures[[j]], "Measure")
      expect_equal(getBMRMeasures(bmr)[[j]], bmr$measures[[j]])
      expect_equal(getBMRMeasureIds(bmr)[[j]],  bmr$measures[[j]]$id)
    }
    for (j in seq_along(bmr$learners)) expect_is(bmr$learners[[j]], "Learner")
    
    # check getBMRPredictions
    preds = getBMRPredictions(bmr, as.df = FALSE)
    expect_true(is.list(preds))
    preds1 = preds[[1L]]
    expect_true(is.list(preds1))
    preds11 = preds1[[1L]]
    expect_is(preds11, "Prediction")
    
    p = getBMRPerformances(bmr, as.df = TRUE)
    expect_is(p, "data.frame")
    expect_true(nrow(p) > 1)
    
    a = getBMRAggrPerformances(bmr, as.df = TRUE)
    expect_is(a, "data.frame")
    expect_true(nrow(a) == 1)
  }

  with_empty_cache({
    with_main_server({
      class.tasks = lapply(c(3064, 3503), getOMLTask)
      regr.tasks = lapply(c(2284, 5046), getOMLTask)
      
      ## Supervised Classification with predict.type = "response"
      #run.class = lapply(class.tasks, function(x) runTaskMlr(x, makeLearner("classif.rpart", predict.type = "response")))
      #run.class.ids = sapply(run.class, function(x) uploadOMLRun(x, tags = "convertBMR"))
      run.class.ids = c(1854986, 1854987)
      ## Supervised Classification tasks with different estimation procedures
      #run.class.prob = lapply(class.tasks, function(x) runTaskMlr(x, makeLearner("classif.rpart", predict.type = "prob")))
      #run.class.prob.ids = sapply(run.class.prob, function(x) uploadOMLRun(x, tags = "convertBMR"))
      run.class.prob.ids = c(1854989, 1854990)
      ## Supervised Regression
      #run.regr = lapply(regr.tasks, function(x) runTaskMlr(x, makeLearner("regr.rpart")))
      #run.regr.ids = sapply(run.regr, function(x) uploadOMLRun(x, tags = "convertBMR"))
      run.regr.ids = c(1854996, 1854998)
      
      ### Supervised Classification tasks with different estimation procedures
      run.class.list = lapply(run.class.ids, getOMLRun)
      bmr = lapply(run.class.list, convertOMLRunToBMR, measures = c("area_under_roc_curve"))
      for(i in 1:length(bmr)) {
        checkBMR(bmr[[i]])
        expect_equal(bmr[[i]]$measures[[1]]$id, "auc")
        expect_numeric(getPredictionProbabilities(getBMRPredictions(bmr[[i]])[[1]][[1]]))
      }
      expect_is(mlr::mergeBenchmarkResults(bmr), "BenchmarkResult")
      
      ### Supervised Classification with predict.type = "response"
      run.class.prob.list = lapply(run.class.prob.ids, getOMLRun)
      bmr = lapply(run.class.prob.list, convertOMLRunToBMR, measures = c("area_under_roc_curve"))
      for(i in 1:length(bmr)) {
        checkBMR(bmr[[i]])
        expect_error(getPredictionProbabilities(getBMRPredictions(bmr[[i]])[[1]][[1]]), "Probabilities not present")
      }
      expect_is(mlr::mergeBenchmarkResults(bmr), "BenchmarkResult")
      
      ### Supervised Regression
      run.regr.list = lapply(run.regr.ids, getOMLRun)
      bmr = lapply(run.regr.list, convertOMLRunToBMR, measures = c("root_mean_squared_error"))
      for(i in 1:length(bmr)) {
        checkBMR(bmr[[i]])
      }
      expect_is(mlr::mergeBenchmarkResults(bmr), "BenchmarkResult")
    })
  })
})
