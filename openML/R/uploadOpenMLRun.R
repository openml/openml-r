#' Upload an OpenML run to the server.
#' 
#' Share a run of an implementation on a given OpenML task.
#' 
#' @param description [\code{\link{OpenMLRun}}]\cr 
#'   An OpenML run description file. Should contain the task and implementation id
#'   and optionally any parameter settings that are specific for this run.
#' @param predictions [\code{data.frame}]\cr
#'   A data.frame with the predictions. Must have the same form as produced by \code{\link{runTask}}.
#' @param session.hash [\code{character(1)}]\cr
#'   A session token returned by \code{\link{authenticateUser}}.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{TRUE}.
#' @export

# FIXME: Is 'description' a path to a file or the content of an XML file? 

uploadOpenMLRun <- function(run.desc, predictions, session.hash, show.info = TRUE) {
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
}