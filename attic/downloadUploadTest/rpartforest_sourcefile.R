library(mlr)
lrn <- makeLearner("classif.rpart")
bagging <- makeBaggingWrapper(lrn, bag.iters = 500)