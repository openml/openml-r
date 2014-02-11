#' Upload an OpenML run to the server.
#' 
#' Share a run of an implementation on a given OpenML task.
#' 
#' @param task [\code{\link{OpenMLTask}}]\cr 
#'   The task.
#' @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr 
#'   The mlr learner, if used. Optional.
#' @param oml.impl [\code{\link{OpenMLImplementation}}]\cr 
#'   The OpenML implementation object of the implementation. Must be stored on the OpenML server.
#' @param predictions [\code{data.frame}]\cr
#'   The predictions. Must have the same form as produced by \code{\link{runTask}}.
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

uploadOpenMLRun <- function(task, mlr.lrn, oml.impl, predictions, session.hash, run.pars = NULL, show.info = TRUE) {
  
  # FIXME: We might want to have a function(task, mlr.lrn, predictions, hash) that uploads both the
  # implementation and the predictions. For this, we need an API call to get the ID of an already
  # registered implementation.
  if(missing(mlr.lrn)) {
    checkArg(run.pars, "list")
  } else {
    checkArg(mlr.lrn, "Learner")
    run.pars <- makeRunParameterList(mlr.lrn)
  }
  checkArg(task, "OpenMLTask")
  checkArg(oml.impl, "OpenMLImplementation")
  checkArg(predictions, "data.frame")
  
  run.desc <- OpenMLRun(
    task.id = as.character(task@task.id), 
    implementation.id = sprintf("%s(%s)", oml.impl@name, oml.impl@version), 
    parameter.settings = run.pars)
  
  description <- tempfile()
  writeOpenMLRunXML(run.desc, description)
  
  output <- tempfile()
  write.arff(predictions, file = output)
  
  file <- tempfile()
  if (show.info) {
    messagef("Uploading run to server.")
    messagef("Downloading response to: %s", file)
  }
  
  url <- getServerFunctionURL("openml.run.upload")
  params <- list(
    description = fileUpload(filename = description),
    predictions = fileUpload(filename = output),
    session_hash = session.hash
  )
  content <- postForm(url, 
    .params = params
  )
  #content <- postForm(url, .params = params, .checkParams = FALSE)
  write(content, file = file)
  # was uploading successful?
  doc <- try(parseXMLResponse(file, "Uploading run", "upload_run"), silent = TRUE)
  # if not, print the error.
  if(is.error(doc)) {
    parseXMLResponse(file, "Uploading run", "response")
  } 
  
  if (show.info) 
    messagef("Run successfully uploaded.")
  
  return(if(!is.error(doc)) xmlRValI(doc, "/oml:upload_run/oml:run_id"))
}