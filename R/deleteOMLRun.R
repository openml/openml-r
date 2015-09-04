#' @title Delete an OpenML run.
#'
#' @description This will delete one of your uploaded \code{\link{OMLRun}s}.
#'
#' @param run.id [\code{integer(1)}]\cr
#'   The run ID. Of course, you can only delete your own runs.
#' @template arg_verbosity
#' @export
deleteOMLRun = function(run.id, verbosity = NULL) {
  run.id = asCount(run.id)

  response = try(doAPICall(api.call = "run", method = "DELETE", id = run.id), silent = TRUE)
  if (is.error(response)) {
    stop("Unknown run. Please check the run ID.")
  }
  doc = parseXMLResponse(response, "Deleting run", "run_delete", as.text = TRUE)
  showInfo(verbosity, "Run %s succesfully deleted.", run.id)
}
