#' @title Upload OpenML Study information.
#'
#' @description
#' A OpenML study is a collection of OpenML objects.
#' If you create a study through the website \url{https://www.openml.org/new/study}, you can also specify an alias which can be used to access the study.
#'
#' @param x [[\code{\link{OMLStudy}}]\cr
#'   Contains the study information that should be uploaded.
#' @template arg_confirm.upload
#' @template arg_verbosity
#' @return [\code{OMLStudy}].
#' @family uploading functions
#' @export
uploadOMLStudy = function(x, confirm.upload = NULL, verbosity = NULL) {
  if (!checkUserConfirmation(type = "study", confirm.upload = confirm.upload)) {
    return(invisible())
  }
  showInfo(verbosity, "Uploading study to server.")

  desc.file = tempfile(fileext = ".xml")
  on.exit(unlink(desc.file))
  writeOMLStudyXML(x, desc.file)

  response = doAPICall(api.call = "study", method = "POST", file = NULL, verbosity = verbosity,
    post.args = list(description = upload_file(path = desc.file)))
  doc = parseXMLResponse(response, "Uploading dataset", c("upload_study", "response"), as.text = TRUE)
  id = xmlOValI(doc, "/oml:upload_study/oml:id")
  # if (!is.null(tags)) tagOMLObject(data.id, object = "data", tags = tags)
  return(invisible(id))
}
