#' @title Run mlr learner on OpenML task.
#'
#' Run task with a specified learner from mlr and produce predictions.
#'
#' @param task [\code{\link{OMLTask}}]\cr
#'   An OpenML task.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Learner from package mlr to run the task.
#' @param remove.const.feats [\code{logical(1)}]\cr
#'   Should constant features be removed?
#'   Default is \code{TRUE}.
#' @param ... [any]\cr
#'   Further arguments that are passed to \code{\link[mlr]{removeConstantFeatures}}.
#' @template arg_verbosity
#' @return List of:
#'   \item{run.pred}{[\code{\link[mlr]{ResamplePrediction}}]\cr
#'     Predictions resulting from the run. These are necessary in order to upload a run.}
#'   \item{mlr.resample.results}{[\code{list}]\cr
#'     The results of the mlr function \code{\link[mlr]{resample}}.\cr
#'     \code{NULL} if \code{return.mlr.results == FALSE}.}
#' @seealso \code{\link{OMLTask}}, \code{\link[mlr]{learners}},
#'   \code{\link{authenticateUser}}, \code{\link[mlr]{resample}}
#' @export
# FIXME: if !return.mlr.results, the output is not a list!
runTask = function(task, learner, remove.const.feats = TRUE, ..., verbosity = NULL) {

  assertClass(task, "OMLTask")
  assertClass(learner, "Learner")
  assertFlag(remove.const.feats)

  if ((task$type == "Supervised Classification" && learner$type != "classif") ||
    (task$type == "Supervised Regression" && learner$type != "regr"))
    stopf("Learner type ('%s') does not correspond to task type ('%s').", task$type, learner$type)

  mlr.task = toMlr(task)

  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  show.info = (verbosity > 0L)

  if (remove.const.feats)
    mlr.task$mlr.task = removeConstantFeatures(mlr.task$mlr.task, show.info = show.info, ...)

  res = resample(learner, mlr.task$mlr.task, mlr.task$mlr.rin, measures = mlr.task$mlr.measures,
    show.info = show.info)
  pred = reformatPredictions(pred = res$pred$data, task = task, orig.lvls = mlr.task$orig.lvls)
  results = list(
    run.pred = pred,
    mlr.resample.results = res
  )
  if (!return.mlr.results) {
    results$mlr.resample.results = NULL
    results = results$run.pred
  }
  return(results)
}

