#' @title Run mlr learner on OpenML task.
#'
#' @description
#' Run task with a specified learner from mlr and produce predictions.
#'
#' @template arg_task
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Learner from package mlr to run the task.
#' @param remove.const.feats [\code{logical(1)}]\cr
#'   Should constant features be removed?
#'   Default is \code{TRUE}.
#' @param drop.levels [\code{logical(1)}]\cr
#'   Drop all empty factor levels in the data?
#'   Default is \code{TRUE}.
#' @template arg_verbosity
#' @param auto.upload [\code{logical(1)}]\cr
#'   Checks whether an \code{\link{OMLFlow}} object containing the passed \code{learner}
#'   was already uploaded to the server.
#'
#'   If it has not been found on the server and \code{auto.upload = TRUE},
#'   a new \code{implementation.id} is assigned and the \code{\link{OMLFlow}} is
#'   automatically uploaded. If the \code{learner} was already uploaded, the
#'   \code{implementation.id} of the respective \code{\link{OMLFlow}} is used.
#'
#'   If \code{auto.upload = FALSE}, only the \code{implementation.id} of an
#'   already uploaded \code{learner} is used and an error is returned if the
#'   \code{learner} was not found on the server.
#' @param resample.extract [\code{function}]\cr
#'   Function used to extract information from a fitted model during the resampling done by \code{mlr}.
#'   Is applied to every \code{\link{WrappedModel}} resulting from calls to \code{\link{train}}
#'   during resampling.
#'   Default is to extract nothing.
#' @param ... [any]\cr
#'   Further arguments that are passed to \code{\link[mlr]{removeConstantFeatures}}.
#' @return [\code{OMLMlrRun}], an \code{\link{OMLRun}} with an additional slot \code{mlr.resample.result}.
#' @seealso \code{\link{getOMLTask}}, \code{\link[mlr]{makeLearner}}
#' @export
runTaskMlr = function(task, learner, remove.const.feats = TRUE, drop.levels = TRUE,
  verbosity = NULL, auto.upload = TRUE, resample.extract, ...) {

  assertClass(task, "OMLTask")
  assertClass(learner, "Learner")
  assertFlag(remove.const.feats)

  if ((task$task.type == "Supervised Classification" && learner$type != "classif") ||
    (task$task.type == "Supervised Regression" && learner$type != "regr") ||
    (task$task.type == "Survival Analysis" && learner$type != "surv") ||
    (task$task.type == "Clustering" && learner$type != "cluster")) {
    stopf("Learner type ('%s') does not correspond to task type ('%s').", task$task.type, learner$type)
  }

  run = makeOMLRun(task.id = task$task.id)
  mlr.task = toMlr(task, verbosity = verbosity)

  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  show.info = (verbosity > 0L)

  if (remove.const.feats)
    mlr.task$mlr.task = removeConstantFeatures(mlr.task$mlr.task, show.info = show.info, ...)

  res = try(resample(learner, mlr.task$mlr.task, mlr.task$mlr.rin, measures = mlr.task$mlr.measures,
    extract = resample.extract, show.info = show.info), silent = TRUE)
  if (!is.error(res)) {
    run$predictions = reformatPredictions(pred = res$pred$data, task = task, orig.lvls = mlr.task$orig.lvls)
    run$mlr.resample.result = res
  } else {
    run$error.message = res[1]
  }
  class(run) = c("OMLMlrRun", "OMLRun")

  run$parameter.setting = makeOMLRunParList(learner)

  # check = checkOMLFlow(learner, verbosity = verbosity)

  # if(check$exists) {
  #   run$implementation.id = xmlOValI(check$doc, "/oml:implementation_exists/oml:id")
  # } else {
  #   if(auto.upload) {
  #     run$implementation.id = uploadOMLFlow(learner, verbosity = verbosity)
  #   } else {
  #     stopf("Flow does not exist, use 'auto.upload = TRUE' to upload it.")
  #   }
  # }
  return(run)
}


# Generate a list of OpenML run parameter settings for a given mlr learner.
#
# @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr
#   The mlr learner.
# @param component [\code{character}]\cr
#   If the learner is a (sub-)component of an implementation, this component's name.
# @return A list of \code{\link{OpenMLRunParameter}s}.
# @examples
# library(mlr)
# lrn = makeLearner("classif.rpart", minsplit = 1)
# bagging = makeBaggingWrapper(lrn, bw.iters = 500)
#
# lrn.par.settings = makeRunParameterList(lrn)
# lrn.par.settings
#
# bagging.par.settings = makeRunParameterList(bagging)
# bagging.par.settings
makeOMLRunParList = function(mlr.lrn, component = NA_character_) {
  assertClass(mlr.lrn, "Learner")
  assertString(component, na.ok = TRUE)

  par.vals = mlr.lrn$par.vals
  par.names = names(mlr.lrn$par.vals)
  par.settings = vector("list", length(par.vals))
  for (i in seq_along(par.vals)) {
    par.settings[[i]] = makeOMLRunParameter(
      name = par.names[i],
      value = as.character(par.vals[[i]]),
      component = component)
  }
  if (!is.null(mlr.lrn$next.learner)) {
    # Use the learner's id (without "classif." or "regr.") as the subcomponent's name...
    # FIXME: check if or make sure that this is correct
    component = strsplit(mlr.lrn$next.learner$id, split = ".", fixed = TRUE)[[1]][2]
    inner.par.settings = makeOMLRunParList(mlr.lrn$next.learner, component = component)
    par.settings = c(par.settings, inner.par.settings)
  }
  return(par.settings)
}
