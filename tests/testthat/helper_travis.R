library(mlr)
if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("APPVEYOR"), "True")) {
  p = normalizePath("~/.openml/cache", mustWork = FALSE)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  setOMLConfig(apikey = Sys.getenv("OPENMLAPIKEY"), cachedir = p, arff.reader = "farff",
    server = "http://test.openml.org/api/v1", confirm.upload = FALSE)
}

# add flows if they are missing
# flows = listOMLFlows(tag = "mlr_test_flows")
# lrn.list = list(
#   makeLearner("classif.rpart", predict.type = "response"),
#   makeLearner("classif.rpart", predict.type = "prob"),
#   makeLearner("classif.logreg", predict.type = "response"),
#   makeLearner("classif.logreg", predict.type = "prob"),
#   makeLearner("classif.lda"),
#   makeLearner("classif.randomForest"),
#   makeFilterWrapper(makeLearner("classif.randomForest"), fw.perc = 0.5, fw.method = "variance"),
#   makeOversampleWrapper(makeLearner("classif.randomForest"), osw.rate = 1),
#   makeImputeWrapper(makeLearner("classif.randomForest"), class = imputeMedian()),
#   makeOversampleWrapper(makeFilterWrapper(makeLearner("classif.randomForest"), fw.method = "variance"), osw.rate = 1),
#   makeLearner("regr.rpart"),
#   makeLearner("regr.lm")
# )
# lrn.ids = unique(unlist(lapply(lrn.list, function(x) paste0("mlr.", getLearnerId(x)))))
#
# if (nrow(flows) < length(lrn.ids)) {
#   flow.ids = lapply(lrn.list, uploadOMLFlow, tag = "mlr_test_flows")
# }

Sys.setenv(NOT_CRAN = "true")
