#' @title Convert an OpenML task to mlr.
#'
#' @description
#' Converts an \code{\link{OMLTask}} to a list of \code{\link[mlr]{Task}},
#' \code{\link[mlr]{ResampleInstance}} and \code{\link[mlr]{Measure}}.
#'
#' @param obj [\code{\link{OMLTask}}]\cr
#'   The OML task object that should be converted.
#' @return [list] A list with the following components:
#' \describe{
#'   \item{mlr.task}{[\code{\link[mlr]{Task}}]}
#'   \item{mlr.rin}{[\code{\link[mlr]{ResampleInstance}}]}
#'   \item{mlr.measures}{[list of \code{\link[mlr]{Measure}s} to optimize for.}
#' }
#' @inheritParams convertOMLDataSetToMlr
#' @family task-related functions
#' @example /inst/examples/convertOMLTaskToMlr.R
#' @export
convertOMLTaskToMlr = function(
  obj,
  ignore.flagged.attributes = TRUE,
  drop.levels = TRUE,
  verbosity = NULL) {
  assertClass(obj, "OMLTask")

  mlr.task = convertOMLDataSetToMlr(obj$input$data.set, obj$task.type,
    obj$input$target.features, ignore.flagged.attributes, drop.levels, verbosity)
  mlr.task$task.desc$id = paste0("OpenML-Task-", obj$task.id)
  mlr.rin = convertOMLSplitsToMlr(obj$input$estimation.procedure, mlr.task, predict = "test")
  # use time as measure and aggregate by sum
  time.measures = list(
    usercpu_time_millis_training = setAggregation(timetrain, test.sum),
    usercpu_time_millis_testing = setAggregation(timepredict, test.sum)
  )
  mlr.measures = append(convertOMLMeasuresToMlr(obj$input$evaluation.measures), time.measures)
  list(mlr.task = mlr.task, mlr.rin = mlr.rin, mlr.measures = mlr.measures)
}
