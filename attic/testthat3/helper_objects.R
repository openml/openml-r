lrn.list = list(
  makeLearner("classif.rpart", predict.type = "response"),
  makeLearner("classif.rpart", predict.type = "prob"),
  makeLearner("classif.logreg", predict.type = "response"),
  makeLearner("classif.logreg", predict.type = "prob"),
  makeLearner("classif.lda"),
  makeLearner("classif.randomForest"),
  makeFilterWrapper(makeLearner("classif.randomForest"), fw.perc = 0.5, fw.method = "variance"),
  makeOversampleWrapper(makeLearner("classif.randomForest"), osw.rate = 1),
  makeImputeWrapper(makeLearner("classif.randomForest"), class = imputeMedian()),
  makeOversampleWrapper(makeFilterWrapper(makeLearner("classif.randomForest"), fw.method = "variance"), osw.rate = 1),
  makeLearner("regr.rpart"),
  makeLearner("regr.lm")
)
names(lrn.list) = vcapply(lrn.list, function(x) paste0(getLearnerId(x), ".", getLearnerPredictType(x)))

flow.ids = vnapply(lrn.list, uploadOMLFlow, tag = "mlr_test_flows")
