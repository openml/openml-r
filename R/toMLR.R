#' @title Convert an OpenML object to mlr.
#'
#' @description This function converts an \code{\link{OMLDataSet}}
#'   or an \code{\link{OMLTask}} into an mlr \code{\link[mlr]{Task}} and, in case of a given
#'   \code{\link{OMLTask}}, various other mlr objects (see below).
#' @param obj [\code{\link{OMLDataSet}} | \code{\link{OMLTask}}]\cr
#'   The object that should be converted. Required.
#' @param target [\code{character}]\cr
#'   The target for the classification/regression task. Default is the
#'   \code{default.target.attribute} of the \code{DataSetDescription}.
#' @param remove.target.NAs [\code{logical(1)}]\cr
#'   Should rows with missing target values be removed? Default is \code{TRUE}. Note, that
#'   the function might fail if you set this to \code{FALSE}.
#' @param ignore.flagged.attributes [\code{logical(1)}]\cr
#'   Should those features that are listed in the data set description's member "ignore.attribute"
#'   be ignored? Default is \code{TRUE}.
#' @return Either a [\code{\link[mlr]{Task}}] or, a list of:
#'   \item{mlr.task}{[\code{\link[mlr]{Task}}]\cr
#'     The task.}
#'   \item{mlr.rin}{[\code{\link[mlr]{ResampleInstance}}]\cr
#'     A server defined resample instance.}
#'   \item{mlr.measures}{[\code{list}]\cr
#'     A list of \code{\link[mlr]{Measure}s} to optimize for.}
#'   \item{original.lvls}{[\code{character}]\cr
#'     All original (possibly empty) levels of the target feature. Empty levels are
#'     removed from the data set during conversion but their names are needed to produce proper
#'     uploadable predictions.}
#' @export
toMlr = function(obj, target, remove.target.NAs, ignore.flagged.attributes) {
  UseMethod("toMlr")
}

#' @rdname toMlr
#' @export
toMlr.OMLTask = function(obj, target = obj$data.set$desc$default.target.attribute,
  remove.target.NAs = TRUE, ignore.flagged.attributes = TRUE) {

  assertSubset(target, obj$data.set$colnames.new, empty.ok = FALSE)
  assertFlag(remove.target.NAs)
  assertFlag(ignore.flagged.attributes)

  task.type = obj$type
  data.set = obj$data.set
  data = obj$data.set$data
  estim.proc = obj$estimation.procedure
  if (remove.target.NAs) {
    tar.na = is.na(data[, target])
    data.set$data = subset(data, !tar.na)
  }
  mlr.task = createMlrTask(data.set, target, task.type, ignore.flagged.attributes)
  mlr.rin = createMlrResampleInstance(estim.proc, mlr.task$mlr.task)
  mlr.measures = createMlrMeasures(obj$evaluation.measures, task.type)
  res = list(mlr.task = mlr.task$mlr.task, mlr.rin = mlr.rin, mlr.measures = mlr.measures)
  res$orig.lvls = mlr.task$orig.lvls
  return(res)
}

#' @rdname toMlr
#' @export
toMlr.OMLDataSet = function(obj, target = obj$desc$default.target.attribute,
  remove.target.NAs = TRUE, ignore.flagged.attributes = TRUE) {

  desc = obj$desc
  
  assertSubset(target, obj$colnames.new, empty.ok = FALSE)
  assertFlag(remove.target.NAs)
  assertFlag(ignore.flagged.attributes)

  data = obj$data
  if (remove.target.NAs) {
    tar.na = is.na(data[, target])
    obj$data = subset(data, !tar.na)
  }
  if (length(target) == 1) {
    task.type = ifelse(is.factor(data[, target]), "Supervised Classification", "Supervised Regression")
  } else {
    stopf("Currently no support for tasks with more than one target column.")
  }
  mlr.task = createMlrTask(obj, target, task.type, ignore.flagged.attributes)
  return(mlr.task$mlr.task)
}

createMlrTask = function(data.set, target, task.type, ignore.flagged.attributes) {
  assertClass(data.set, "OMLDataSet")
  data = data.set$data
  desc = data.set$desc
  orig.lvls = NULL
  if (task.type == "Supervised Classification") {
    orig.lvls = levels(data[, target])
  }
  if (!is.na(desc$ignore.attribute) && ignore.flagged.attributes) {
    inds = which(data$colnames.new %in% desc$ignore.attribute)
    data = data[, -inds]
  }
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

  # FIXME: match using gsub("[[:space:]_]", "")
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
