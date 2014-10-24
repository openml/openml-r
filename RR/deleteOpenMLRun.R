#' @title Delete an OpenML run.
#'
#' @description This will delete one of your uploaded \code{\link{OpenMLRun}s}.
#'
#' @param run.id [\code{integer(1)}]\cr
#'   The run ID. Of course, you can only delete your own runs.
#' @template arg_hash
#' @template arg_showinfo
#' @export
deleteOpenMLRun = function(run.id, session.hash, show.info = getOpenMLOption("show.info")) {
  run.id = asCount(run.id)
  assertString(session.hash)
  
  url = getAPIURL("openml.run.delete")
  response = postForm(url,
    session_hash = session.hash,
    run_id = run.id
  )
  doc = parseXMLResponse(response, "Deleting run", "run_delete", as.text = TRUE)
  
  if (show.info)
    catf("Run %s succesfully deleted.", run.id)
}
