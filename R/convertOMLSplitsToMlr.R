convertOMLSplitsToMlr = function(estim.proc, mlr.task, predict = "both") {
  type = estim.proc$type
  n.repeats = estim.proc$parameters[["number_repeats"]]
  n.folds = estim.proc$parameters[["number_folds"]]
  percentage = as.numeric(estim.proc$parameters[["percentage"]])
  data.splits = estim.proc$data.splits
  #FIXME: I think the server always prdoced stratified resampling for classif? we need to check this.
  # if so, we need to set that property, but only after the split sets for mlr have been overwritten.
  # otherwise in some case some mlr sanity check apparently gets triggered.
  # FIXME: more resampling
  if (type == "crossvalidation") {
    if (n.repeats == 1L)
      mlr.rdesc = mlr::makeResampleDesc("CV", iters = n.folds, predict = predict)
    else
      mlr.rdesc = mlr::makeResampleDesc("RepCV", reps = n.repeats, folds = n.folds, predict = predict)
    mlr.rin = mlr::makeResampleInstance(mlr.rdesc, task = mlr.task)
  } else if (type == "holdout") {
    mlr.rdesc = mlr::makeResampleDesc("Holdout", split = 1 - percentage / 100, predict = predict)
    mlr.rin = mlr::makeResampleInstance(mlr.rdesc, task = mlr.task)
    n.folds = 1
  } else if (type == "leaveoneout") {
    mlr.rdesc = mlr::makeResampleDesc("LOO", predict = predict)
    mlr.rin = mlr::makeResampleInstance(mlr.rdesc, task = mlr.task)
    n.folds = nrow(mlr.task$env$data)
  } else {
    stopf("Unsupported estimation procedure type: %s", type)
  }
  iter = 1L
  for (r in seq_len(n.repeats)) {
    for (f in seq_len(n.folds)) {
      d = subset(data.splits, rep == r & data.splits$fold == f)
      mlr.rin$train.inds[[iter]] = subset(d, type == "TRAIN")$rowid
      mlr.rin$test.inds[[iter]] = subset(d, type == "TEST")$rowid
      iter = iter + 1L
    }
  }
  return(mlr.rin)
}
