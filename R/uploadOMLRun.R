#' Upload an OpenML run to the server.
#'
#' Share a run of an implementation on a given OpenML task.
#'
#' @param run [\code{\link{OMLRun}}]\cr
#'   The run that should be uploaded.
#' @template arg_implementation.id
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{invisible(numeric(1))}]. The run ID.
#' @export
uploadOMLRun = function(run, implementation.id, session.hash = getSessionHash(), verbosity = NULL) {
  assertClass(run, "OMLRun")
  if (is.na(run$implementation.id)) {
    if (!missing(implementation.id)) {
      run$implementation.id = asCount(implementation.id)
    } else stop("Please provide an 'implementation.id'")
  } else {
    if (!missing(implementation.id)) stop("This run has already an 'implementation.id'.")
  }
  if (is.na(run$error.message)) {
    assertDataFrame(run$predictions)
  } else {
    assertString(run$error.message)
  }

  description = tempfile()
  on.exit(unlink(description))
  writeOMLRunXML(run, description)

  url = getAPIURL("openml.run.upload")

  if (!is.null(run$predictions)) {
    output = tempfile()
    on.exit(unlink(output), add = TRUE)
    write.arff(run$predictions, file = output)

    content = downloadXML(url, NULL, verbosity,
      description = fileUpload(filename = description),
      predictions = fileUpload(filename = output),
      session_hash = session.hash
    )
  } else {
    content = downloadXML(url, NULL, verbosity,
      description = fileUpload(filename = description),
      session_hash = session.hash
    )
  }
  # was uploading successful?
  doc = try(parseXMLResponse(content, "Uploading run", "upload_run", as.text = TRUE), silent = TRUE)
  
  # if not, print the error.
  if (is.error(doc)) {
    parseXMLResponse(content, "Uploading run", "response")
  }
  # else, return the run.id invisibly
  showInfo(verbosity, "Run successfully uploaded.")

  return(invisible(xmlRValI(doc, "/oml:upload_run/oml:run_id")))
}