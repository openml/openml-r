#' Upload an OpenML run to the server.
#' 
#' Share a run of an implementation on a given OpenML task.
#' 
#' @param task [\code{\link{OpenMLTask}}]\cr 
#'   The task.
#' @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr 
#'   The mlr learner, if used. Optional.
#' @param impl.id [\code{\link{character}}]\cr 
#'   The ID of the OpenML implementation that is stored on the OpenML server.
#' @param predictions [\code{data.frame}]\cr
#'   The predictions. Must have the same form as produced by \code{\link{runTask}}. If a fatal error occured while 
#'   computing predictions, you have the possibility to leave this out and pass an error message instead (as parameter
#'   error.msg).
#' @param error.msg [\code{try-error}]\cr
#'   An optional error message. If this is not empty, the predictions (if present) are ignored.
#' @param session.hash [\code{character(1)}]\cr
#'   A session token returned by \code{\link{authenticateUser}}.
#' @param run.pars [\code{list}]\cr
#'   If no mlr learner was used, this is a list of all \code{\link{OpenMLRunParameter}s}.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{TRUE}.
#' @return [\code{numeric(1)} or \code{NULL}]. Run ID if the run was uploaded succesfully.
#' @export

# FIXME: Rewrite description as soon as the function's parameters are definite.

uploadOpenMLRun = function(task, mlr.lrn, impl.id, predictions, error.msg, session.hash, 
  run.pars = NULL, clean.up = TRUE, show.info = TRUE) {
  
  if (missing(mlr.lrn)) {
    assertList(run.pars)
  } else {
    assertClass(mlr.lrn, "Learner")
    run.pars = makeRunParameterList(mlr.lrn)
  }
  assertClass(task, "OpenMLTask")
  impl.id = asCount(impl.id)
    
  if (missing(error.msg)) {
    assertDataFrame(predictions)
  } else {
    assertString(error.msg)
  }
  
  run.desc = makeOpenMLRun(
    task.id = task$id, 
    implementation.id = impl.id, 
    parameter.settings = run.pars
  )
  
  if (!missing(error.msg))
    run.desc$error.message = error.msg
  
  description = tempfile()
  on.exit(if (clean.up) unlink(description))
  writeOpenMLRunXML(run.desc, description)
  
  file = tempfile()
  on.exit(if (clean.up) unlink(file), add = TRUE)
  if (show.info) {
    messagef("Uploading run to server.")
    messagef("Downloading response to: %s", file)
  }
  
  url = getServerFunctionURL("openml.run.upload")
  
  if (!missing(predictions)) {
    output = tempfile()
    on.exit(if (clean.up) unlink(output), add = TRUE)
    write.arff(predictions, file = output) 
    
    content = postForm(url, 
      description = fileUpload(filename = description),
      predictions = fileUpload(filename = output),
      session_hash = session.hash
    )
  } else {
    content = postForm(url, 
      description = fileUpload(filename = description),
      session_hash = session.hash
    )
  }
  write(content, file = file)
  # was uploading successful?
  doc = try(parseXMLResponse(file, "Uploading run", "upload_run"), silent = TRUE)
  # if not, print the error.
  if (is.error(doc)) {
    parseXMLResponse(file, "Uploading run", "response")
  } 
  
  if (show.info) 
    messagef("Run successfully uploaded.")
  
  return(if (!is.error(doc)) xmlRValI(doc, "/oml:upload_run/oml:run_id"))
}
