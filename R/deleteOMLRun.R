#' @title Delete an OpenML run.
#'
#' @description This will delete one of your uploaded \code{\link{OMLRun}s}.
#'
#' @param run.id [\code{integer(1)}]\cr
#'   The run ID. Of course, you can only delete your own runs.
#' @template arg_hash
#' @template arg_verbosity
#' @export
deleteOMLRun = function(run.id, session.hash = getSessionHash(), verbosity = NULL) {
  run.id = asCount(run.id)
  assertString(session.hash)

  url = getAPIURL("openml.run.delete")
  response = try(postForm(url, session_hash = session.hash, run_id = run.id), silent = TRUE)
  if (is.error(response)) {
    stop("Unknown run. Please check the run ID.")
  }
  doc = parseXMLResponse(response, "Deleting run", "run_delete", as.text = TRUE)
  showInfo(verbosity, "Run %s succesfully deleted.", run.id)
}
