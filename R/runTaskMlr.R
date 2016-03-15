#' @title Run mlr learner on OpenML task.
#'
#' @description
#' Run task with a specified learner from \pkg{mlr} and produce predictions.
#'
#' @template arg_task
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Learner from package mlr to run the task.
#' @template arg_verbosity
#' @param seed [\code{numeric(1)}]\cr
#'   Set a seed to reproduce this run.
#'   Default is \code{1}.
#' @param scimark.vector [\code{numeric(6)}]\cr
#'   Optional vector of performance measurements computed by the scientific SciMark
#'   benchmark. May be computed using the \pkg{rscimark} R package.
#'   Default is \code{NULL}, which means no performance measurements.
#' @param ... [any]\cr
#'   Further arguments that are passed to \code{\link{convertOMLTaskToMlr}}.
#' @return [\code{list}] Named list with the following components:
#' \describe{
#'   \item{run}{The \code{\link{OMLRun}} object.}
#'   \item{bmr}{Benchmark result returned by \code{\link[mlr]{benchmark}}.}
#'   \item{flow}{The generated \code{\link{OMLFlow}} object.}
#' }
#' @seealso \code{\link{getOMLTask}}, \code{\link[mlr]{makeLearner}}
#' @export
runTaskMlr = function(task, learner, verbosity = NULL, seed = 1, scimark.vector = NULL, ...) {
  assert(checkString(learner), checkClass(learner, "Learner"))
  if (is.character(learner))
    learner = makeLearner(learner)
  assertClass(task, "OMLTask")
  assertChoice(task$task.type, c("Supervised Classification", "Supervised Regression"))
  if (!is.null(scimark.vector))
    assertNumeric(scimark.vector, lower = 0, len = 6, finite = TRUE, any.missing = FALSE, all.missing = FALSE)

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
  bmr = benchmark(learner, z$mlr.task, z$mlr.rin, measures = z$mlr.measures, show.info = show.info)
  res = bmr$results[[1]][[1]]

  # add error message
  tr.err = unique(res$err.msgs$train)
  pr.err = unique(res$err.msgs$predict)
  if (any(!is.na(tr.err))) {
    tr.msg = paste0("Error in training the model: \n ", collapse(tr.err, sep = "\n "))
  } else {
    tr.msg = NULL
  }
  if (any(!is.na(pr.err))) {
    pr.msg = paste0("Error in making predictions: \n ", collapse(pr.err, sep = "\n "))
  } else {
    pr.msg = NULL
  }
  msg = paste0(tr.msg, pr.msg)

  # create run
  run = makeOMLRun(task.id = task$task.id, error.message = ifelse(length(msg) == 0, NA_character_, msg))
  # FIXME: allow list of results?
  run$predictions = reformatPredictions(res$pred$data, task)

  # Add parameter settings and seed
  parameter.setting = makeOMLRunParList(learner)
  # FIXME: modify seed.pars names because there are learners that have "seed" as parameter setting (e.g. classif.randomForestSRC)?
  # names(seed.pars) = paste0("R.", names(seed.pars))
  seed.setting = lapply(seq_along(seed.pars), function(x) {
    makeOMLRunParameter(
      name = names(seed.pars[x]),
      value = as.character(seed.pars[x]),
      component = NA_character_
    )
  })
  run$parameter.setting = append(parameter.setting, seed.setting)
  flow = createOMLFlowForMlrLearner(learner)
  run$flow = flow

  par.names = extractSubList(run$parameter.setting, "name")
  if (length(par.names) != length(unique(par.names)))
    stop("duplicated names in 'parameter.setting' and/or 'seed.setting'")
  # run$flow$source.path = createLearnerSourcefile(learner)
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
  if (!is.null(scimark.vector)) {
    run$scimark.vector = scimark.vector
  }
  return(list(run = run, bmr = bmr, flow = flow))
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

  ps = mlr.lrn$par.set$pars
  par.vals = mlr.lrn$par.vals
  par.names = names(mlr.lrn$par.vals)
  # get defaults for par.vals that have been set
  par.defaults = getDefaults(mlr.lrn$par.set)
  # store only par.vals that are different from default values
  par.ind = vlapply(par.names, function(x) !isTRUE(all.equal(par.defaults[[x]] , par.vals[[x]])))
  par.vals = par.vals[par.ind]
  par.names = par.names[par.ind]

  par.settings = vector("list", length(par.vals))
  for (i in seq_along(par.vals)) {
    psi = ps[[par.names[i]]]
    # FIXME: what happens with parameters that are vectors (or not scalars, e.g. deeplearning)?
    val = paramValueToString(psi, par.vals[[i]])
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
