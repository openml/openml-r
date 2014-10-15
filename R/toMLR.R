#' @title Convert an OpenML object to mlr.
#'   
#' @description This function converts an \code{\link{OpenMLDataSetDescription}}
#'   or an \code{\link{OpenMLTask}} into an mlr \code{\link[mlr]{Task}}. In
#'   the latter case, a list is returned with the following elements:\cr 
#'   \code{mlr.task} -- the \code{\link[mlr]{Task}},\cr \code{mlr.rin} -- a
#'   \code{\link[mlr]{ResampleInstance}} which was defined by the server,\cr 
#'   \code{mlr.measures} -- a list of \code{\link[mlr]{Measure}s} to optimize
#'   for,\cr \code{orig.lvls} -- a \code{character} vector containing all
#'   original (possibly empty) levels of the target feature. Empty levels are
#'   removed from the data set during conversion.
#' @param obj [\code{\link{OpenMLDataSetDescription}} | \code{\link{OpenMLTask}}]\cr 
#'   The object that should be converted. Required.
#' @param target [\code{character}]\cr 
#'   The target for the classification/regression task. Default is the 
#'   \code{default.target.attribute} of the \code{DataSetDescription}.
#' @param remove.target.NAs [\code{logical(1)}]\cr
#'   Should rows with missing target values be removed? Default is \code{TRUE}. Note, that 
#'   the function might fail if you set this to \code{FALSE}.
#' @return [\code{\link[mlr]{Task}}]
#' @export
toMlr = function(obj, target, remove.target.NAs) {
  requirePackages("mlr", why = "toMlr")
  UseMethod("toMlr")
}

#' @rdname toMlr
#' @export
toMlr.OpenMLTask = function(obj, target = obj$data.desc$default.target.attribute, 
  remove.target.NAs = TRUE) {
  
  assertSubset(target, obj$data.desc$new.col.names, empty.ok = FALSE)
  assertFlag(remove.target.NAs)
  task.type = obj$type
  data.set.desc = obj$data.desc
  data = obj$data.desc$data.set
  estim.proc = obj$estimation.procedure
  if (remove.target.NAs) {
    tar.na = is.na(data[, target])
    data = subset(data, !tar.na)
  }
  mlr.task = createMlrTask(data, target, task.type)
  mlr.rin = createMlrResampleInstance(estim.proc, mlr.task$mlr.task)
  mlr.measures = createMlrMeasures(obj$evaluation.measures, task.type)
  res = list(mlr.task = mlr.task$mlr.task, mlr.rin = mlr.rin, mlr.measures = mlr.measures)
  res$orig.lvls = mlr.task$orig.lvls
  return(res)
}

#' @rdname toMlr
#' @export
toMlr.OpenMLDataSetDescription = function(obj, target = obj$default.target.attribute,
  remove.target.NAs = TRUE) {
  
  assertSubset(target, obj$new.col.names, empty.ok = FALSE)
  assertFlag(remove.target.NAs)
  data = obj$data.set
  if (remove.target.NAs) {
    tar.na = is.na(data[, target])
    data = subset(data, !tar.na)
  }
  if (length(target) == 1) {
    task.type = ifelse(is.factor(data[, target]), "Supervised Classification", "Supervised Regression")
  } else {
    stopf("Currently no support for tasks with more than one target column.")
  }
  mlr.task = createMlrTask(data, target, task.type)
  return(mlr.task$mlr.task)
}

createMlrTask = function(data, target, task.type) {
  assertDataFrame(data)
  orig.lvls = NULL
  if (task.type == "Supervised Classification") {
    orig.lvls = levels(data[, target])
  }
  #FIXME some data sets have empty factor levels, mlr does not like this
  # fix this for now by removing
  data = droplevels(data)
  if (task.type == "Supervised Classification") {
    mlr.task = makeClassifTask(data = data, target = target)
  } else if (task.type == "Supervised Regression") {
    mlr.task = makeRegrTask(data = data, target = target)
  } else {
    stopf("Encountered currently unsupported task type: %s", task.type)
  }
  return(list(mlr.task = mlr.task, orig.lvls = orig.lvls))
}

createMlrResampleInstance = function(estim.proc, mlr.task) {
  type = estim.proc$type
  n.repeats = estim.proc$parameters[["number_repeats"]]
  n.folds = estim.proc$parameters[["number_folds"]]
  percentage = as.numeric(estim.proc$parameters[["percentage"]])
  data.splits = estim.proc$data.splits
  #FIXME: I think the server always prdoced stratified resampling for classif? we need to check this.
  # if so, we need to set that property, but only after the split sets for mlr have been overwritten.
  # otherwise in some case some mlr sanity check apparently gets triggered.
  stratify = (mlr.task$task.desc$type == "classif")
  # FIXME : more resampling
  if (type == "crossvalidation") {
    if (n.repeats == 1L)
      mlr.rdesc = makeResampleDesc("CV", iters = n.folds)
    else
      mlr.rdesc = makeResampleDesc("RepCV", reps = n.repeats, folds = n.folds)
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
}

# FIXME: add more metrics/measures.
createMlrMeasures = function(measures, type) {
  assertCharacter(measures, any.missing = FALSE)
  
  getMlrMeasures = function(measures, measure.list) {
    mlr.measures = vector("list", length(measures))
    for (i in seq_along(measures)) {
      which.contain = lapply(measure.list, function(x) measures[i] %in% x)
      measure.name = names(which.contain[which.contain == TRUE])
      if (is.null(measure.name)) {
        stopf("Unsupported evaluation measure: %s", measure.name)
      }
      mlr.measures[[i]] = get(measure.name)
    }
    return(mlr.measures)
  }
  
  classif.list = list(
    mmce = c("mean_absolute_error", "mean absolute error"),
    auc = c("area_under_roc_curve", "area under roc curve"),
    timetrain = c("build_cpu_time", "build cpu time"),
    f1 = c("f_measure", "f measure"),
    mcc = c("matthews_correlation_coefficient", "matthews correlation coefficient"),
    ppv = "precision",
    acc = c("predictive_accuracy", "predictive accuracy"),
    tpr = "recall")
  regr.list = list(
    rmse = c("root_mean_squared_error", "root mean squared error"),
    mae = c("mean_absolute_error", "mean absolute error"),
    auc = c("area_under_roc_curve", "area under roc curve"),
    timetrain = c("build_cpu_time", "build cpu time"),
    f1 = c("f_measure", "f measure"),
    mcc = c("matthews_correlation_coefficient", "matthews correlation coefficient"),
    ppv = "precision",
    acc = c("predictive_accuracy", "predictive accuracy"),
    tpr = "recall")
  
  if (type == "Supervised Classification") {
    return(getMlrMeasures(measures, classif.list))
  } else if (type == "Supervised Regression") {
    return(getMlrMeasures(measures, regr.list))
  } else {
    stopf("Unsupported task type: %s", type)
  }
}
