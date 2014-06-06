#' OpenMLRun
#'
#' This class of objects contains the information describing an openML run.
#' 
#' Objects can be created by calls of the form \code{OpenMLRun(...)}.
#' The objects contain information on ... .
#'
#'$section Slots: 
#'  \describe{
#'    \item{\code{task.id}}{[\code{character}]\cr
#'    The id of the solved task.}
#'    \item{\code{implementation.id}}{[\code{character}]\cr
#'    The id of the used implementation.}
#'    \item{\code{parameter.settings}}{[\code{list}]\cr
#'    Optional parameter settings of this run. A list contatining 
#'    \code{\link{OpenMLRunParameter}s}.}
#'  }
#'
#' @name OpenMLRun
#' @rdname OpenMLRun
#' @aliases OpenMLRun-class
#' @exportClass OpenMLRun

# FIXME: Do we still need this? This is a subclass of OpenMLRunResults. Maybe more convenient though.
setClass("OpenMLRun", representation(
  task.id = "character",
  implementation.id = "character",
  error.message = "character",
  parameter.settings = "list"
))


# ***** Constructor *****
OpenMLRun <- function(task.id, implementation.id, error.message = character(0), parameter.settings=list()) {
  new("OpenMLRun",
    task.id = task.id,
    implementation.id = implementation.id,
    error.message = error.message,
    parameter.settings = parameter.settings
  )
}

# ***** Methods *****

setMethod("show", "OpenMLRun", function(object) {
  catf('** Information on an OpenML Run **\n')
  catf('Task ID           :: %s', object$task.id)
  catf('Implementation ID :: %s', object$implementation.id)
  if (length(object$error.message) > 0) {
    catf('Error message     :: %s', object$error.message)
  }
  #FIXME reimplent
  if (length(object$parameter.settings) > 0) {
    cat('Parameter Settings used on the Run:\n')
    print(object$parameter.settings)
  }
  cat('\n')
})




