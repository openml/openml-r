#' @title Run mlr learner on OpenML task.
#'
#' @description
#' Run task with a specified learner from mlr and produce predictions.
#'
#' @template arg_task
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Learner from package mlr to run the task.
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
runTaskMlr = function(task, learner, verbosity = NULL, auto.upload = TRUE, resample.extract = NULL, ...) {

  assertClass(task, "OMLTask")
  assertClass(learner, "Learner")

  run = makeOMLRun(task.id = task$task.id)
  z = convertOMLTaskToMlr(task, verbosity = verbosity, ...)

  # get mlr show.info from verbosity level
  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  show.info = (verbosity > 0L)

  res = resample(learner, z$mlr.task, z$mlr.rin, measures = z$mlr.measures,
    extract = resample.extract, show.info = show.info)
  run$predictions = reformatPredictions(res$pred$data, task)
  run$mlr.resample.result = res
  run = addClasses(run, "OMLMlrRun")

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
