#' @title Convert an OpenML task to mlr.
#'
#' @param obj [\code{\link{OMLTask}}]\cr
#'   The object that should be converted.
#' @return A list of:
#'   \item{mlr.task}{[\code{\link[mlr]{Task}}]}
#'   \item{mlr.rin}{[\code{\link[mlr]{ResampleInstance}}]}
#'   \item{mlr.measures}{[list of \code{\link[mlr]{Measure}s} to optimize for.}
#' @inheritParams convertOMLDataSetToMlr
#' @export
convertOMLTaskToMlr = function(obj, ignore.flagged.attributes = TRUE, drop.levels = TRUE, verbosity = NULL) {
  assertClass(obj, "OMLTask")

  # FIXME: here it is wrong that we take the taget from the dset, must be from task!
  mlr.task = convertOMLDataSetToMlr(obj$input$data.set, obj$task.type,
    obj$input$data.set$target.features, ignore.flagged.attributes, drop.levels, verbosity)
  mlr.rin = convertOMLSplitsToMlr(obj$input$estimation.procedure, mlr.task)
  mlr.measures = convertOMLMeasuresToMlr(obj$input$evaluation.measures, mlr.task)
  list(mlr.task = mlr.task, mlr.rin = mlr.rin, mlr.measures = mlr.measures)
}

