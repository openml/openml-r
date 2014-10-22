#' @title Reset an OpenML run.
#'
#' @description This will reset the status of one of your uploaded \code{\link{OpenMLRun}s} to
#'   "unprocessed", meaning that the server's evaluation engine will pick up on it again. This is
#'   useful if a temporary server problem occured so that your run has not been processed correctly.
#'
#' @param run.id [\code{integer(1)}]\cr
#'   The run ID. Note, that you can only reset your own runs.
#' @param session.hash [\code{character(1)}]\cr
#'   A session token returned by \code{\link{authenticateUser}}.
#' @template arg_showinfo
#' @export
resetOpenMLRun = function(run.id, session.hash, show.info = getOpenMLOption("show.info")) {
  run.id = asCount(run.id)
  assertString(session.hash)

  url = getAPIURL("openml.run.reset")
  response = postForm(url,
    session_hash = session.hash,
    run_id = run.id
  )
  doc = parseXMLResponse(response, "Resetting run", "run_reset", as.text = TRUE)

  if (show.info)
    catf("Run %s succesfully reset.", run.id)
}
