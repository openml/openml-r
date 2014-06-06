#' OpenMLTaskResults
#'
#' This class of objects contains information on all \code{\link{OpenMLRunResults}} for a certain 
#' \code{\link{OpenMLTask}}.
#' 
#' The objects contain information on ... .
#'
#'
#' @section Slots: 
#'  \describe{
#'    \item{\code{task.id}}{[\code{character}]\cr
#'    ID of the OpenMLTask.}
#'    \item{\code{task.type.id}}{[\code{character}]\cr
#'    ID of the task type.}
#'    \item{\code{input.data}}{[\code{character}]\cr
#'    ID of the data set that belongs to the task.}
#'    \item{\code{task.name}}{[\code{character}]\cr
#'    A name describing the task shortly.}
#'    \item{\code{estimation.procedure}}{[\code{character}]\cr
#'    Information on the task's estimation procedure.}
#'    \item{\code{metrics}}{[\code{data.frame}]\cr
#'    A data.frame of the metrics of all runs that were uploaded for this task.}
#'  }
#'
#' @name OpenMLTaskResults
#' @rdname OpenMLTaskResults
#' @aliases OpenMLTaskResults-class
#' @exportClass OpenMLTaskResults

# --------------------------------------------------------------
# class def
setClass("OpenMLTaskResults", representation(
  task.id = "character",
  task.name = "character",
  task.type.id = "character",
  input.data = "character",
  estimation.procedure = "character",
  metrics = "data.frame"
))


# --------------------------------------------------------------
# constructor function
OpenMLTaskResults = function(
  task.id = character(0L),
  task.name = character(0L),
  task.type.id  = character(0L),
  input.data = character(0L),
  estimation.procedure = character(0L),
  metrics = data.frame()
) {
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
setMethod("show", "OpenMLTaskResults", function(object) {
  catNotEmpty = function(s, val) {
    if (val != "" && length(val) > 0) 
      catf("%s %s", s, val)
  }
  
  ## General info
  catf('\n** Task Information **')
   
  catNotEmpty('Task ID              :: ', object$task.id)
  catNotEmpty('Task Name            :: ', object$task.name)
  catNotEmpty('Task Type ID         :: ', object$task.type.id)  
  catNotEmpty('Input Data           :: ', object$input.data)
  catNotEmpty('Estimation Procedure :: ', object$estimation.procedure)
  
  ## Metrics
  catf('\n** Metrics **')
  print(object$metrics)
})
