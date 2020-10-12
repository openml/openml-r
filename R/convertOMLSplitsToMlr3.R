convertOMLSplitsToMlr3 = function(estim.proc, mlr.task, predict = "both") {
  type = estim.proc$type
  n.repeats = estim.proc$parameters[["number_repeats"]]
  n.folds = estim.proc$parameters[["number_folds"]]
  percentage = as.numeric(estim.proc$parameters[["percentage"]])
  data.splits = estim.proc$data.splits
  stratified = estim.proc$parameters[["stratified_sampling"]]
  stratified = ifelse(is.null(stratified), FALSE, stratified == "true")

  if (type == "crossvalidation") {
    if (n.repeats == 1L)
      mlr.rdesc = mlr3::rsmp("cv", folds = n.folds, stratify = stratified)
    else
      mlr.rdesc = mlr3::rsmp("repeated_cv", reps = n.repeats, folds = n.folds, stratify = stratified)
    mlr.rin = mlr.rdesc$instantiate(mlr.task)
  } else if (type == "holdout") {
    mlr.rdesc = mlr3::rsmp("holdout")
    mlr.rin = mlr.rdesc$instantiate(task = mlr.task)
    n.folds = 1
  } else {
    stopf("Unsupported estimation procedure type: %s", type)
  }
  return(mlr.rin)
}
