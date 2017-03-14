context("convertOMLRunToBMR")

test_that("convertOMLRunToBMR", {
  with_empty_cache({
    with_main_server({
      ### Supervised Classification tasks with different estimation procedures
      # set.seed(1)
      # tid = c(3632, 4367) #4322, 1817
      # tasks = lapply(tid, getOMLTask)
      # runs = lapply(tasks, function(x) runTaskMlr(x, makeLearner("classif.rpart", predict.type = "prob")))
      # run.ids = sapply(runs, function(x) uploadOMLRun(x, tags = "convertBMR"))
      # 
      checkBMR = function(bmr) {
        expect_is(bmr, "BenchmarkResult")
        #res.classes = c("character", "character", "data.frame", "data.frame", "numeric",
        #  "Prediction", "list", "data.frame", "list", "numeric", "Learner" )
        #for (j in seq_along(res.classes)) expect_is(bmr$results[[1]][[1]][[j]], res.classes[j])
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
      
      run.ids = 543387:543388 # 543387:543390
      run.list = lapply(run.ids, getOMLRun)
      bmr = lapply(run.list, convertOMLRunToBMR, measures = c("area_under_roc_curve"))
      for(i in 1:length(bmr)) {
        checkBMR(bmr[[i]])
        expect_equal(bmr[[i]]$measures[[1]]$id, "auc")
        expect_numeric(getPredictionProbabilities(getBMRPredictions(bmr[[i]])[[1]][[1]]))
      }
      expect_is(mlr::mergeBenchmarkResults(bmr), "BenchmarkResult")
      
      ### Supervised Classification with predict.type = "response"
      # runs = lapply(tasks, function(x) runTaskMlr(x, makeLearner("classif.rpart", predict.type = "response")))
      # run.ids = sapply(runs, function(x) uploadOMLRun(x, tags = "convertBMR"))
      # 
      run.ids = 543394:543395 # c(543394:543396, 543398)
      run.list = lapply(run.ids, getOMLRun)
      bmr = lapply(run.list, convertOMLRunToBMR, measures = c("area_under_roc_curve"))
      for(i in 1:length(bmr)) {
        checkBMR(bmr[[i]])
        expect_error(getPredictionProbabilities(getBMRPredictions(bmr[[i]])[[1]][[1]]), "Probabilities not present")
      }
      
      ### Supervised Regression
      # set.seed(1)
      # tid = c(4903, 5217)
      # tasks = lapply(tid, getOMLTask)
      # runs = lapply(tasks, function(x) runTaskMlr(x, makeLearner("regr.rpart")))
      # run.ids = sapply(runs, function(x) uploadOMLRun(x, tags = "convertBMR"))
      # 
      run.ids = 568501:568502
      run.list = lapply(run.ids, getOMLRun)
      bmr = lapply(run.list, convertOMLRunToBMR, measures = c("root_mean_squared_error"))
      for(i in 1:length(bmr)) {
        checkBMR(bmr[[i]])
      }
    })
  })
})
