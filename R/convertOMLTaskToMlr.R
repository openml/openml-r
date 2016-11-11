#' @title Convert an OpenML task to mlr.
#'
#' @description
#' Converts an \code{\link{OMLTask}} to a list of \code{\link[mlr]{Task}},
#' \code{\link[mlr]{ResampleInstance}} and \code{\link[mlr]{Measure}}.
#'
#' @param obj [\code{\link{OMLTask}}]\cr
#'   The OML task object that should be converted.
#' @param mlr.task.id [\code{character(1)}]\cr
#'   Id string for \code{\link[mlr]{Task}} object.
#'   The strings \code{<oml.data.name>}, \code{<oml.data.id>}, \code{<oml.data.version>}
#'   and \code{<oml.task.id>} will be replaced by their respective values contained
#'   in the \code{\link{OMLTask}} object.
#'   Default is \code{<oml.data.name>}.
#' @param measures [\code{\link[mlr]{Measure}}]\cr
#'  Additional measures that should be computed.
#' @return [list] A list with the following objects:
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
  measures = NULL,
  mlr.task.id = "<oml.data.name>",
  ignore.flagged.attributes = TRUE,
  drop.levels = TRUE,
  verbosity = NULL) {
  assertClass(obj, "OMLTask")
  assert(checkClass(measures, "Measure"), checkList(measures, types = "Measure"), checkNull(measures))
  if (inherits(measures, "Measure")) measures = list(measures)

  mlr.task.id = gsub("<oml.task.id>", obj$task.id, mlr.task.id)
  mlr.task = convertOMLDataSetToMlr(obj = obj$input$data.set,
    mlr.task.id = mlr.task.id,
    task.type = obj$task.type,
    target = obj$input$target.features,
    ignore.flagged.attributes = ignore.flagged.attributes,
    drop.levels = drop.levels,
    verbosity = verbosity)
  mlr.rin = convertOMLSplitsToMlr(obj$input$estimation.procedure, mlr.task, predict = "test")
  # use time as measure and aggregate by sum
  time.measures = list(
    usercpu_time_millis_training = mlr::setAggregation(mlr::timetrain, mlr::test.sum),
    usercpu_time_millis_testing = mlr::setAggregation(mlr::timepredict, mlr::test.sum)
  )
  mlr.measures = c(convertOMLMeasuresToMlr(obj$input$evaluation.measures), time.measures, measures)
  list(mlr.task = mlr.task, mlr.rin = mlr.rin, mlr.measures = mlr.measures)
}
