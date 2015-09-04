#' @title Upload an OpenML dataset to the server.
#'
#' @note
#' Assumes that the given dataset is ARFF.
#' Maybe we can add dataframe to ARFF conversion here?
#'
#' @param data.set [\code{\link{data.frame}}]\cr
#'   The dataset that should be uploaded.
#' @param data.file [\code{character(1)}]\cr
#'   Data file.
#' @template arg_verbosity
#' @return [\code{invisible(numeric(1))}]. The id of the data (\code{did}).
#' @export
uploadOMLDataSet = function(data.set, data.file, verbosity = NULL) {
  if (is.null(data.file)) {
    stopf("Please provide dataset file.")
  }
  assertFile(file)
  data.set$file.md5 = digest(file = data.file)

  # TODO: check if dataset already exists on server?

  tmp.file = tempfile()
  on.exit(unlink(tmp.file))
  writeOMLDataSetXML(data.set, tmp.file)

  showInfo(verbosity, "Uploading data set to server.")
  #showInfo(verbosity, "Downloading response to: %s", file)

  response = doAPICall(api.call = "data", method = "POST", file = NULL, verbosity = verbosity,
      post.args = list(description = upload_file(path = tmp.file)))
  response = parseXMLResponse(response, "Uploading dataset", c("upload_dataset", "response"), as.text = TRUE)
  data.id = xmlOValI(doc, "/oml:upload_dataset/oml:id")
  showInfo(verbosity, "Data set successfully uploaded. Data set ID: %i", data.id)
  return(invisible(data.id))
}
