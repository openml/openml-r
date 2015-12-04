#' @title Run mlr learner on OpenML task.
#'
#' @description
#' Run task with a specified learner from mlr and produce predictions.
#'
#' @template arg_task
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Learner from package mlr to run the task.
#' @template arg_verbosity
#' @param seed [\code{numeric(1)}]\cr
#'   Set a seed to reproduce this run.
#'   Default is \code{1}.
#' @param ... [any]\cr
#'   Further arguments that are passed to \code{\link{convertOMLTaskToMlr}}.
#' @return [\code{OMLMlrRun}], an \code{\link{OMLRun}} with additional slots: \code{mlr.benchmark.result} and \code{flow}.
#' @seealso \code{\link{getOMLTask}}, \code{\link[mlr]{makeLearner}}
#' @export
runTaskMlr = function(task, learner, verbosity = NULL, seed = 1, ...) {
  assertClass(learner, "Learner")
  assertClass(task, "OMLTask")
  assertChoice(task$task.type, c("Supervised Classification", "Supervised Regression"))

  # set default evaluation measure for classification and regression
  if (task$input$evaluation.measures == "") {
    if (task$task.type == "Supervised Classification")
      task$input$evaluation.measures = "predictive_accuracy"
    else
      task$input$evaluation.measures =  "root_mean_squared_error"
  }

  # get mlr show.info from verbosity level
  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  show.info = (verbosity > 0L)

  # Create mlr task with estimation procedure and evaluation measure
  z = convertOMLTaskToMlr(task, verbosity = verbosity, ...)

  # Create seed info and set this seed
  seed.pars = setNames(c(seed, RNGkind()), c("seed", "kind", "normal.kind"))
  do.call("set.seed", as.list(seed.pars))

  # Create OMLRun
  run = makeOMLRun(task.id = task$task.id)
  res = benchmark(learner, z$mlr.task, z$mlr.rin, measures = z$mlr.measures, show.info = show.info)
  # FIXME: allow list of results?
  run$predictions = reformatPredictions(res$results[[1]][[1]]$pred$data, task)
  run$mlr.benchmark.result = res
  # Add parameter settings and seed
  parameter.setting = makeOMLRunParList(learner)
  seed.setting = lapply(seq_along(seed.pars), function(x) {
    makeOMLRunParameter(
      name = names(seed.pars[x]),
      value = as.character(seed.pars[x]),
      component = NA_character_
    )
  })
  run$parameter.setting = append(parameter.setting, seed.setting)
  run$flow = createOMLFlowForMlrLearner(learner)
  run$flow$source.path = createLearnerSourcefile(learner)
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
  run = addClasses(run, "OMLMlrRun")
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
    # FIXME: if it is possible to convert parameter to character, do this. What happens with vectors?
    val = try(as.character(par.vals[[i]]), silent = TRUE)
    if (is.error(val)) val = par.vals[[i]]
    par.settings[[i]] = makeOMLRunParameter(
      name = par.names[i],
      value = val, #par.vals[[i]],
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
