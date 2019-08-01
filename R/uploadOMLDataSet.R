#' @title Upload a data set to the OpenML server.
#'
#' @description
#' Share a data set by uploading it to the OpenML server.
#'
#' @note
#' This function will reset the cache of \code{\link{listOMLDataSets}} on success.
#'
#' @param x [\code{\link[mlr]{Task}}|[\code{\link{OMLDataSet}}]\cr
#'   Contains the data set that should be uploaded.
#' @template arg_upload_tags
#' @template arg_description
#' @template arg_confirm.upload
#' @template arg_verbosity
#' @return [\code{invisible(numeric(1))}].
#'   The ID of the data (\code{data.id}).
#' @family uploading functions
#' @family data set-related functions
#' @export
uploadOMLDataSet = function(x, tags = NULL, description = NULL, confirm.upload = NULL, verbosity = NULL) {
  UseMethod("uploadOMLDataSet")
}

#' @export
uploadOMLDataSet.OMLDataSet = function(x, tags = NULL, description = NULL, confirm.upload = NULL, verbosity = NULL) {
  if (!checkUserConfirmation(type = "dataset", confirm.upload = confirm.upload)) {
    return(invisible())
  }

  # if (length(x$desc$default.target.attribute) > 1) {
  #   target.df = x$data[, x$desc$default.target.attribute]
  #   assertDataFrame(target.df, types = "logical")
  # }

  desc.file = tempfile(fileext = ".xml")
  on.exit(unlink(desc.file))
  writeOMLDataSetXML(x$desc, desc.file)

  output = tempfile(fileext = ".arff")
  on.exit(unlink(output), add = TRUE)
  arff.writer(x$data, file = output)

  showInfo(verbosity, "Uploading data set to server.")

  response = doAPICall(api.call = "data", method = "POST", file = NULL, verbosity = verbosity,
    post.args = list(description = upload_file(path = desc.file),
                     dataset = upload_file(path = output)))
  doc = parseXMLResponse(response, "Uploading dataset", c("upload_data_set", "response"), as.text = TRUE)
  data.id = xmlOValI(doc, "/oml:upload_data_set/oml:id")
  showInfo(verbosity, "Data set successfully uploaded. Data set ID: %i", data.id)
  if (!is.null(tags)) tagOMLObject(data.id, object = "data", tags = tags)
  forget(listOMLDataSets)
  return(invisible(data.id))
}

#' @export
uploadOMLDataSet.Task = function(x, tags = NULL, description = NULL, confirm.upload = NULL, verbosity = NULL) {
  x = convertMlrTaskToOMLDataSet(x, description = description)
  uploadOMLDataSet.OMLDataSet(x, confirm.upload = NULL, verbosity = verbosity)
}
