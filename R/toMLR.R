#' Convert an OpenML task to an MLR task object.
#' 
#' @param task [\code{\linkS4class{OpenMLTask}}]\cr 
#'   An OpenML task object. Required.
#' @return [\code{\link[mlr]{SupervisedTask}}] 
#' @export 

toMLR = function(task) {
  checkArg(task, "OpenMLTask")
  requirePackages("mlr", why="toMLR")
  task.type = task$type
  data.set.desc = task$data.desc
  data = task$data.desc$data.set
  target = task$target.features
  
  #FIXME some data sets have empty factor levels, mlr does not like this
  # fix this for now by removing
  data = droplevels(data)
  
  # FIXME: hack to convert bad feature names
  feature.ind = which(colnames(data) %nin% target)
  feature.names = colnames(data)[feature.ind]
  feature.names = str_replace_all(feature.names, pattern=c("\\-"), replacement="_")  
  feature.names = str_replace_all(feature.names, pattern=c("/"), replacement="_")  
  colnames(data)[feature.ind] = feature.names
  
  estim.proc = task$estimation.procedure
  if (task.type == "Supervised Classification") {
    mlr.task = makeClassifTask(data = data, target = target)
  } else if (task.type == "Supervised Regression") {
    mlr.task = makeRegrTask(data = data, target = target)
  } else {
    stopf("Encountered currently unsupported task type: %s", task.type)
  }
  mlr.rin = createMLRResampleInstance(estim.proc, mlr.task)
  mlr.measures = createMLRMeasures(task$task.evaluation.measures, task.type)
  list(mlr.task = mlr.task, mlr.rin = mlr.rin, mlr.measures = mlr.measures)
}

createMLRResampleInstance = function(estim.proc, mlr.task) {
  type = estim.proc$type
  n.repeats = estim.proc$parameters[["number_repeats"]]
  n.folds = estim.proc$parameters[["number_folds"]]
  percentage = as.numeric(estim.proc$parameters[["percentage"]])
  data.splits = estim.proc$data.splits
  #FIXME why is stratify TRUE here? does the server always prdoced stratified
  # resampling for classif? check this
  stratify = (mlr.task$task.desc$type == "classif")
  # FIXME : more resampling
  if (type == "crossvalidation") {
    if (n.repeats == 1L)
      mlr.rdesc = makeResampleDesc("CV", iters = n.folds, stratify = stratify)
    else 
      mlr.rdesc = makeResampleDesc("RepCV", reps = n.repeats, folds = n.folds, stratify = stratify)
    mlr.rin = makeResampleInstance(mlr.rdesc, task = mlr.task)  
  } else if (type == "holdout") {
    mlr.rdesc = makeResampleDesc("Holdout", split = 1 - percentage/100)
    mlr.rin = makeResampleInstance(mlr.rdesc, task = mlr.task)
    n.folds = 1
  } else {
    stopf("Unsupported estimation procedure type: %s", type)
  }
  iter = 1L
  for (r in 1:n.repeats) {
    for (f in 1:n.folds) {
      d = subset(data.splits, rep ==  r & data.splits$fold == f)
      mlr.rin$train.inds[[iter]] = subset(d, type == "TRAIN")$rowid 
      mlr.rin$test.inds[[iter]] = subset(d, type == "TEST")$rowid
      iter = iter + 1L
    }
  }
  return(mlr.rin)
  #print(table(data.splits$rep, data.splits$fold, data.splits$type))

}

# FIXME: add more metrics/measures.
createMLRMeasures = function(measures, type) {
  lapply(measures, function(m) {
    if(type == "Supervised Classification") {
      switch(m, 
        mean_absolute_error = mmce,
        area_under_roc_curve = auc,
        build_cpu_time = timetrain,     
        f_measure = f1,
        matthews_correlation_coefficient = mcc,
        precision = ppv,
        predictive_accuracy = acc,
        recall = tpr,
        stopf("Unsupported evaluation measure: %s", m)     
      )
    } else {
      switch(m, 
        root_mean_squared_error = rmse,
        mean_absolute_error = mae,
        area_under_roc_curve = auc,
        build_cpu_time = timetrain,     
        f_measure = f1,
        matthews_correlation_coefficient = mcc,
        precision = ppv,
        predictive_accuracy = acc,
        recall = tpr,
        stopf("Unsupported evaluation measure: %s", m)     
      )
    } 
  })
}
