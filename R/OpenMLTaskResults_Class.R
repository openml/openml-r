#' OpenMLTaskResults
#'
#' @title Construct OpenMLTask.
#'   
#' @param task.id [\code{character}]\cr
#'   ID of the OpenMLTask.
#' @param task.type.id [\code{character(1)}]\cr
#'   ID of the task type.
#' @param input.data [\code{character(1)}]\cr
#'   ID of the data set that belongs to the task.
#' @param task.name [\code{character(1)}]\cr
#'   A name describing the task shortly.
#' @param estimation.procedure [\code{character}]\cr
#'   Information on the task's estimation procedure.
#' @param metrics [\code{data.frame}]\cr
#'   A data.frame of the metrics of all runs that were uploaded for this task.
#' @export
#' @aliases OpenMLTaskResults OpenMLTaskResults-class
makeOpenMLTaskResults = function(task.id, task.name, task.type.id = NA_character_, 
  input.data = NA_character_, estimation.procedure = NA_character_, metrics
) {
  
  assertString(task.id)
  assertString(task.name)
  assertString(task.type.id, na.ok = TRUE)
  assertString(input.data, na.ok = TRUE)
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
print.OpenMLTaskResults = function(x, ...)  {
  catNotNA = function(s, val) {
    if (!is.na(val)) 
      catf("%s %s", s, val)
  }
  
  ## General info
  catf('\n** Task Information **')
   
  catNotNA('Task ID              :: ', x$task.id)
  catNotNA('Task Name            :: ', x$task.name)
  catNotNA('Task Type ID         :: ', x$task.type.id)  
  catNotNA('Input Data           :: ', x$input.data)
  catNotNA('Estimation Procedure :: ', x$estimation.procedure)
  
  ## Metrics
  catf('\n** Metrics **')
  # Do not print confusion matrices and os information to keep it clear
  print(subset(x$metrics, select = -c(confusion_matrix, os_information)))
}
