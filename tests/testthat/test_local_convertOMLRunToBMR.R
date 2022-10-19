# test_that("convertOMLRunToBMR", {
#   with_test_cache({
#     class.tasks = lapply(c(19, 20), getOMLTask)
#     regr.tasks = lapply(c(631, 632), getOMLTask)
# 
#     # ## Supervised Classification with predict.type = "response"
#     # run.class = lapply(class.tasks, function(x) runTaskMlr(x, makeLearner("classif.rpart", predict.type = "response")))
#     # run.class.ids = sapply(run.class, function(x) uploadOMLRun(x, tags = "convertBMR"))
#     # ## Supervised Classification tasks with different estimation procedures
#     # run.class.prob = lapply(class.tasks, function(x) runTaskMlr(x, makeLearner("classif.rpart", predict.type = "prob")))
#     # run.class.prob.ids = sapply(run.class.prob, function(x) uploadOMLRun(x, tags = "convertBMR"))
#     # ## Supervised Regression
#     # run.regr = lapply(regr.tasks, function(x) runTaskMlr(x, makeLearner("regr.rpart")))
#     # run.regr.ids = sapply(run.regr, function(x) uploadOMLRun(x, tags = "convertBMR"))
# 
#     ### Supervised Classification tasks with different estimation procedures
#     run.class.list = lapply(c(219, 220), getOMLRun)
#     bmr = lapply(run.class.list, convertOMLRunToBMR, measures = "area_under_roc_curve")
#     for (i in 1:length(bmr)) {
#       checkBMR(bmr[[i]])
#       expect_equal(bmr[[i]]$measures[[1]]$id, "auc")
#       expect_error(getPredictionProbabilities(getBMRPredictions(bmr[[i]])[[1]][[1]]), "Probabilities not present")
#     }
#     expect_s3_class(mlr::mergeBenchmarkResults(bmr), "BenchmarkResult")
# 
#     ### Supervised Classification with predict.type = "response"
#     run.class.prob.list = lapply(c(221, 222), getOMLRun)
#     bmr = lapply(run.class.prob.list, convertOMLRunToBMR, measures = "area_under_roc_curve")
#     for (i in 1:length(bmr)) {
#       checkBMR(bmr[[i]])
#       expect_data_frame(getPredictionProbabilities(getBMRPredictions(bmr[[i]])[[1]][[1]]))
#     }
#     expect_s3_class(mlr::mergeBenchmarkResults(bmr), "BenchmarkResult")
# 
#     ### Supervised Regression
#     run.regr.list = lapply(c(223, 224), getOMLRun)
#     bmr = lapply(run.regr.list, convertOMLRunToBMR, measures = "root_mean_squared_error")
#     for (i in 1:length(bmr)) {
#       checkBMR(bmr[[i]])
#     }
#     expect_s3_class(mlr::mergeBenchmarkResults(bmr), "BenchmarkResult")
#   })
# })
