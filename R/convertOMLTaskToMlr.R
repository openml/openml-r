#' @title Convert an OpenML task to mlr.
#'
#' @description Converts an \code{\link{OMLTask}} to a list of \code{\link[mlr]{Task}},
#' \code{\link[mlr]{ResampleInstance}} and \code{\link[mlr]{Measure}}.
#'
#' @param obj [\code{\link{OMLTask}}]\cr
#'   The object that should be converted.
#' @return A list of:
#'   \item{mlr.task}{[\code{\link[mlr]{Task}}]}
#'   \item{mlr.rin}{[\code{\link[mlr]{ResampleInstance}}]}
#'   \item{mlr.measures}{[list of \code{\link[mlr]{Measure}s} to optimize for.}
#' @inheritParams convertOMLDataSetToMlr
#' @family task related functions
#' @export
convertOMLTaskToMlr = function(obj, ignore.flagged.attributes = TRUE, drop.levels = TRUE, verbosity = NULL) {
  assertClass(obj, "OMLTask")

  # FIXME: here it is wrong that we take the taget from the dset, must be from task!
  mlr.task = convertOMLDataSetToMlr(obj$input$data.set, obj$task.type,
    obj$input$data.set$target.features, ignore.flagged.attributes, drop.levels, verbosity)
  mlr.task$task.desc$id = paste0("OpenML-Task-", obj$task.id)
  mlr.rin = convertOMLSplitsToMlr(obj$input$estimation.procedure, mlr.task, predict = "test")
  mlr.measures = append(convertOMLMeasuresToMlr(obj$input$evaluation.measures, mlr.task),
    list(usercpu_time_millis_training = timetrain, usercpu_time_millis_testing = timepredict))
  list(mlr.task = mlr.task, mlr.rin = mlr.rin, mlr.measures = mlr.measures)
}
