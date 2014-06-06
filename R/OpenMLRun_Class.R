#'    \code{\link{OpenMLRunParameter}s}.}


#' @title Construct OpenMLRun.
#'
#' @param task.id [\code{character(1)}]\cr
#'    The id of the task.
#' @param implementation.id [\code{character(1)}]\cr
#'   The id of the used implementation.
#' @param error.message [\code{character(1)}]\cr
#'   Potential error message generated during run.
#'   Default is NA, which means no error occurred.
#' @param parameter.settings [named \code{list}]\cr
#'    Optional parameter settings for this run.
#'    Default is empty list.
#' @export
makeOpenMLRun = function(task.id, implementation.id, error.message = NA_character_, parameter.settings = list()) {
  assertString(task.id)
  assertString(implementation.id)
  assertString(error.message, na.ok = TRUE)
  assertList(parameter.settings)
  makeS3Obj("OpenMLRun",
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




