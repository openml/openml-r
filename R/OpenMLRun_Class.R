#    \code{\link{OpenMLRunParameter}s}.}


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
#' @aliases OpenMLRun
makeOpenMLRun = function(task.id, implementation.id, error.message = NA_character_, parameter.settings = list()) {
  task.id = asCount(task.id)
  implementation.id = asCount(implementation.id)
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

#' @export
print.OpenMLRun = function(x, ...)  {
  catf('\nOpenML Run :: (Task ID = %i, Flow ID = %s)', x$task.id, x$implementation.id)
  if (!is.na(x$error.message)) {
    catf('\tError Message: %s', x$error.message)
  }
  if (length(x$parameter.settings) > 0) {
    cat('\n\tParameter Settings used on this Run:\n')
    pars.names = extractSubList(x$parameter.settings, "name")
    pars.vals = extractSubList(x$parameter.settings, "value", simplify = FALSE)
    names(pars.vals) = pars.names
    for (i in seq_along(x$parameter.settings)) {
      comp = x$parameter.settings[[i]]$component
      if (!is.na(comp)) {
        pars.vals[[i]] = sprintf("%s (Component '%s')", pars.vals[i], comp)
      }
    }
    BBmisc:::prettyPrint(pars.vals, prefix = "\t\t", sep = " = ", name.align = "right")
  }
  cat('\n')
}



