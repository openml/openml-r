#' Upload an OpenML dataset to the server. Assumes that the given dataset is ARFF.
#' Maybe we can add dataframe to ARFF conversion here?
#'
#' @param x [\code{\link{data.frame}}]\cr
#'   The dataset that should be uploaded.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{invisible(numeric(1))}]. The id of the data (\code{data.id}).
#' @export

uploadOMLDataSet = function(x, session.hash, verbosity, file) {
  UseMethod("uploadOMLDataSet")
}

#' @export
uploadOMLDataset = function(x, session.hash = getSessionHash(),
  verbosity = NULL, datafile = NULL) {
  if (is.null(file)) {
    stopf("Please provide dataset file.")
  }
  if (!is.null(file)) {
    assertFile(file)
    x$file.md5 = digest(file = datafile)
  }
  # TODO: check if dataset already exists on server?

  file = tempfile()
  on.exit(unlink(file))
  writeOMLDataSetXML(x, file)

  showInfo(verbosity, "Uploading data set to server.")
  showInfo(verbosity, "Downloading response to: %s", file)

  url = getAPIURL("openml.data.upload")
  params = list(session_hash = session.hash, description = fileUpload(filename = file))
  response = postForm(url, .params = params, .checkParams = FALSE)
  doc = parseXMLResponse(response, "Uploading dataset", c("upload_dataset", "response"), as.text = TRUE)
  data.id = xmlOValI(doc, "/oml:upload_dataset/oml:id")
  showInfo(verbosity, "Data set successfully uploaded. Data set ID: %i", data.id)
  return(data.id)
}