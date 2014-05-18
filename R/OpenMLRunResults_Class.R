#' OpenMLRunResults
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
#'    \item{\code{uploader}}{[\code{character}]\cr
#'    The uploader's OpenML user ID.}
#'    \item{\code{implementation.id}}{[\code{character}]\cr
#'    ID of the implementation used for this run.}
#'    \item{\code{setup.id}}{[\code{character}]\cr
#'    ID of the setup that belongs to this run.}
#'    \item{\code{parameters}}{[\code{character}]\cr
#'    A named character vector of the parameter settings used for this run (if they differ from the 
#'    default settings).}
#'    \item{\code{metrics}}{[\code{data.frame}]\cr
#'    A list of the metrics that are computed by the server based on this run's predictions.}
#'  }
#'
#' @name OpenMLRunResults
#' @rdname OpenMLRunResults
#' @aliases OpenMLRunResults-class
#' @exportClass OpenMLRunResults


# FIXME: add data.set.desc and pred.desc
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
  
  catNotEmpty('Run ID            :: ', object@run.id)
  catNotEmpty('Task ID           :: ', object@task.id)
  catNotEmpty('User ID           :: ', object@uploader)
  catNotEmpty('Implementation ID :: ', object@implementation.id)
  
  ## FIXME: Add parameters.
  
  ## Metrics
  catf('\n** Metrics **')
  
  #if(length(object@metrics) > 0) {
  #  for(i in seq_along(object@metrics)) {
  #    catNotEmpty('Name              :: ', object@metrics[[i]]$name)
  #    catNotEmpty('Value             :: ', object@metrics[[i]]$value)
  #    catNotEmpty('Label             :: ', object@metrics[[i]]$label)
  #    cat("\n")
  #  }
  #}
  print(object@metrics)
})

