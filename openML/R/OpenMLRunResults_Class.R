#' OpenMLImplementation
#'
#' This class of objects contains information on the results of an \code{\link{OpenMLRun}}.
#' 
#' The objects contain information on ... .
#'
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{run.id}}{[\code{character}]\cr
#'    ID of the OpenMLRun.}
#'    \item{\code{task.id}}{[\code{character}]\cr
#'    ID of the OpenMLTask that belongs to this run.}
#'    \item{\code{user.id}}{[\code{character}]\cr
#'    The uploader's OpenML user ID.}
#'    \item{\code{implementation.id}}{[\code{character}]\cr
#'    ID of the implementation used for this run.}
#'    \item{\code{parameters}}{[\code{list}]\cr
#'    A list of the parameter settings used for this run (if they differ from the default settings).}
#'    \item{\code{metrics}}{[\code{list}]\cr
#'    A list of the metrics that are computed by the server based on this run's predictions.}
#'  }
#'
#' @name OpenMLRunResults
#' @rdname OpenMLRunResults
#' @aliases OpenMLRunResults-class
#' @exportClass OpenMLRunResults

# --------------------------------------------------------------
# class def
setClass("OpenMLRunResults", representation(
  run.id = "character",
  task.id = "character",
  user.id = "character",
  implementation.id = "character",
  parameters = "character",
  metrics = "list"
))


# --------------------------------------------------------------
# constructor function
OpenMLRunResults <- function(
  run.id = "",
  task.id = "",
  user.id = "",
  implementation.id = "",
  parameters = "",
  metrics = list()
) {
  new("OpenMLRunResults",
      run.id = run.id,
      task.id = task.id,
      user.id = user.id,
      implementation.id = implementation.id,
      parameters = parameters,
      metrics = metrics
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
  
  catNotEmpty('Run ID            :: ', object@run.id)
  catNotEmpty('Task ID           :: ', object@task.id)
  catNotEmpty('User ID           :: ', object@user.id)
  catNotEmpty('Implementation ID :: ', object@implementation.id)
  
  ## FIXME: Add parameters.
  
  ## Metrics
  catf('\n** Metrics **')
  
  if(length(object@metrics) > 0) {
    for(i in seq_along(object@metrics)) {
      catNotEmpty('Name              :: ', object@metrics[[i]]$name)
      catNotEmpty('Value             :: ', object@metrics[[i]]$value)
      catNotEmpty('Label             :: ', object@metrics[[i]]$label)
      cat("\n")
    }
  }
})

