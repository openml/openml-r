#' OpenMLTaskResults
#'
#' @title Construct OpenMLTaskResults.
#'
#' @description Objects of this class are returned by \code{\link{downloadOpenMLTaskResults}}. All
#'   members are filled in by the server.
#'
#' @param task.id [\code{integer(1)}]\cr
#'   ID of the OpenML task.
#' @param task.name [\code{character(1)}]\cr
#'   A name describing the task shortly.
#' @param task.type.id [\code{integer(1)}]\cr
#'   ID of the task type.
#' @param input.data [\code{integer(1)}]\cr
#'   ID of the data set that belongs to the task.
#' @param estimation.procedure [\code{character(1)}]\cr
#'   The task's estimation procedure.
#' @param metrics [\code{data.frame}]\cr
#'   A data.frame of the metrics of all runs that were uploaded for this task.
#' @export
#' @aliases OpenMLTaskResults
#' @seealso \code{\link{downloadOpenMLTaskResults}}
makeOpenMLTaskResults = function(task.id, task.name, task.type.id = NA_integer_,
  input.data = NA_integer_, estimation.procedure = NA_character_, metrics) {

  assertCount(task.id)
  assertString(task.name)
  assertCount(task.type.id, na.ok = TRUE)
  assertCount(input.data, na.ok = TRUE)
  assertString(estimation.procedure, na.ok = TRUE)
  assertDataFrame(metrics)

  makeS3Obj("OpenMLTaskResults",
      task.id = task.id,
      task.name = task.name,
      task.type.id = task.type.id,
      input.data = input.data,
      estimation.procedure = estimation.procedure,
      metrics = metrics
  )
}

# ***** Methods *****

# show
#' @export
print.OpenMLTaskResults = function(x, printMetrics = FALSE, ...)  {
  catNotNA = function(s, val) {
    if (!is.na(val))
      catf("%s %s", s, val)
  }

  ## General info
  catf('\nTask Results :: (Task ID = %i, Data ID = %i)', x$task.id, x$input.data)
  catNotNA('\tTask Type ID        : ', x$task.type.id)
  catNotNA('\tEstimation Procedure: ', x$estimation.procedure)

  if (printMetrics) {
    cat('\n\tMetrics             :\n\n')
    print(x$metrics[, colnames(x$metrics) %nin% c("confusion_matrix", "os_information")])
  }
}
