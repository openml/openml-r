#' OpenMLRunResults
#'
#' This class of objects contains information on the results of an \code{\link{OpenMLRun}}.
#' The objects contain information on... 
#' 
#' @section Slots: 
#'   \describe{
#'     \item{\code{run.id}}{[\code{numeric(1)}]\cr
#'       ID of the run. Added by server. Ignored when uploading a run.}
#'     \item{\code{uploader}}{[\code{numeric(1)}]\cr   
#'       ID of the user that uploaded the run. Added by server. Ignored when uploading a run.}
#'     \item{\code{task.id}}{[\code{numeric(1)}]\cr
#'       ID of the task that is solved in this run. This ID is given in the task description.} 
#'     \item{\code{implementation.id}}{[\code{character(1)}]\cr
#'       ID of the implementation used to solve the task. Returned by the API when you first upload the 
#'       implementation, or given in the implementation description when you download an existing implementation. }
#'     \item{\code{setup.id}}{[\code{numeric(1)}]\cr
#'       Unique ID of the used setup. Ignored when uploading a run (i.e., it will be searched based 
#'       on the parameter settings).} 
#'     \item{\code{error.message}}{[\code{character(1)}]\cr
#'       Whenever an error occurs during the run, this can be reported here.}
#'     \item{\code{parameter.setting}}{[\code{list}]\cr
#'       A list of \code{\link{OpenMLRunParameter}s} containing information on the parameter settings.}
#'     \item{\code{input.data}}{[\code{\link{OpenMLData}}]\cr
#'       All data that served as input for the run. Added by server. Ignored when uploading.}
#'     \item{\code{output.data}}{[\code{\link{OpenMLData}}]\cr
#'       All data that was the output of this run, i.e., predictions, evaluation scores. 
#'       Most of this will be added by the server, but users can also provide evaluation scores for their 
#'       own evaluation measures.}
#'   }
#'
#' @name OpenMLRunResults
#' @rdname OpenMLRunResults
#' @aliases OpenMLRunResults-class
#' @exportClass OpenMLRunResults


# FIXME: add setup.string
# --------------------------------------------------------------
# class def
setClass("OpenMLRunResults", representation(
  run.id = "numeric",
  uploader = "numeric",
  task.id = "numeric",
  implementation.id = "character",
  setup.id = "numeric",
  error.message = "character",
  parameter.setting = "list",
  input.data = "OpenMLData",
  output.data = "OpenMLData"
))

# --------------------------------------------------------------
# constructor function
OpenMLRunResults <- function(
  run.id = numeric(0L),
  uploader = numeric(0L),
  task.id = numeric(0L),
  implementation.id = character(0L),
  setup.id = numeric(0L),
  error.message = character(0L),
  parameter.setting = list(),
  input.data = OpenMLData(),
  output.data = OpenMLData()
) {
  new("OpenMLRunResults",
      run.id = run.id,
      uploader = uploader,
      task.id = task.id,
      implementation.id = implementation.id,
      setup.id = setup.id,
      error.message = error.message,
      parameter.setting = parameter.setting,
      input.data = input.data,
      output.data = output.data
  )
}

# ***** Methods *****

# show
setMethod("show", "OpenMLRunResults", function(object) {
  catNotEmpty <- function(s, val) {
    if (val != "") 
      catf("%s %s", s, val)
  }
  
  ## General info
  catf('\n** Run Information **')
  
  catNotEmpty('Run ID            :: ', object$run.id)
  catNotEmpty('Task ID           :: ', object$task.id)
  catNotEmpty('User ID           :: ', object$uploader)
  catNotEmpty('Implementation ID :: ', object$implementation.id)
  
  ## FIXME: Add parameters.
  
  ## Metrics
  catf('\n** Metrics **')
  
  #if(length(object$metrics) > 0) {
  #  for(i in seq_along(object$metrics)) {
  #    catNotEmpty('Name              :: ', object$metrics[[i]]$name)
  #    catNotEmpty('Value             :: ', object$metrics[[i]]$value)
  #    catNotEmpty('Label             :: ', object$metrics[[i]]$label)
  #    cat("\n")
  #  }
  #}
  print(object$output.data)
})

