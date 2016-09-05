#' @title Upload an OpenML.
#'
#' @description
#' Share a flow by uploading it to the OpenML server.
#'
#' @note
#' This function will reset the cache of \code{\link{listOMLFlows}} on success.
#'
#' @param x [\code{\link{OMLFlow}}|\code{\link[mlr]{Learner}}]\cr
#'   The flow that should be uploaded.
#' @template arg_upload_tags
#' @template arg_verbosity
#' @param sourcefile [\code{character(1)}]\cr
#'   The file path to the flow (not needed for \code{\link[mlr]{Learner}}).
#' @param binaryfile [\code{character(1)}]\cr
#'   The file path to the flow (not needed for \code{\link[mlr]{Learner}}).
#' @return [\code{invisible(numeric)}].
#'   The ID of the flow (\code{flow.id}). If there are more componets in the flow, than a vector of IDs.
#' @family uploading functions
#' @export
uploadOMLFlow = function(x, tags = NULL, verbosity = NULL, sourcefile, binaryfile) {
  UseMethod("uploadOMLFlow")
}

#' @export
uploadOMLFlow.OMLFlow = function(x, tags = NULL, verbosity = NULL, sourcefile = NULL, binaryfile = NULL) {
  # upload components as flows if there are some
  # if (length(x$components) > 0) {
  #   tmp = uploadOMLFlow(x$components[[1]])
  #   if (length(x$components) > 1) {
  #     for(i in 2:length(x$components)) tmp = c(uploadOMLFlow(x$components[[i]]), tmp)
  #   }
  # } else tmp = NULL

  check = checkOMLFlow(x, verbosity = FALSE)
  doc = check$doc
  if (check$exists) {
    flow.id = xmlOValI(doc, "/oml:flow_exists/oml:id")
    showInfo(verbosity, "Flow already exists (Flow ID = %i).", flow.id)
    return(flow.id)
  }
  file = tempfile(fileext = ".xml")
  on.exit(unlink(file))

  if (!checkUserConfirmation(type = "flow")) {
    return(invisible())
  }

  writeOMLFlowXML(x, file)

  showInfo(verbosity, "Uploading flow to server.")
  showInfo(verbosity, "Downloading response to: %s", file)

  #url = getAPIURL("flow/")
  params = list(description = upload_file(path = file))

  # if binary.path is given (and binaryfile is empty), upload binary.path, otherwise upload binaryfile
  if (testFile(x$binary.path) & !testFile(binaryfile)) binaryfile = x$binary.path
  if (testFile(binaryfile)) {
    x$binary.md5 = digest(file = binaryfile)
    params$binary = upload_file(path = binaryfile)
  }
  if (testFile(x$source.path) & !testFile(sourcefile)) sourcefile = x$source.path
  if (testFile(sourcefile)) {
    x$source.md5 = digest(file = sourcefile)
    params$source = upload_file(path = sourcefile)
  }
#   sourcefile.exists = !(is.null(sourcefile) || is.na(sourcefile))
#   if (!(is.null(x$source.path) || is.na(x$source.path)) & !sourcefile.exists)
#     sourcefile = x$source.path
#   if (!(is.null(sourcefile) || is.na(sourcefile))){
#     assertFileExists(sourcefile)
#     x$source.md5 = digest(file = sourcefile)
#     params$source = upload_file(path = sourcefile)
#   }

  response = doAPICall(api.call = "flow", method = "POST", file = NULL,
    verbosity = verbosity, post.args = params)

  # response = postForm(url, .params = params, .checkParams = FALSE)
  doc = parseXMLResponse(response, "Uploading flow", c("upload_flow", "response"), as.text = TRUE)
  flow.id = xmlOValI(doc, "/oml:upload_flow/oml:id")
  showInfo(verbosity, "Flow successfully uploaded. Flow ID: %i", flow.id)
  if (!is.null(tags)) tagOMLObject(flow.id, object = "flow", tags = tags)
  forget(listOMLFlows)
  return(invisible(flow.id))
}

#' @export
uploadOMLFlow.Learner = function(x, tags = NULL,
  verbosity = NULL, sourcefile = NULL, binaryfile = NULL) {
  flow = convertMlrLearnerToOMLFlow(x)

  flow.id = uploadOMLFlow(flow, sourcefile = sourcefile, binaryfile = binaryfile, verbosity = verbosity)
  return(flow.id)
}

# FIXME: remove this when uploading flows without sourcefile are possible (and use setup.string instead)
# createLearnerSourcefile = function(x){
#   sourcefile = file.path(tempdir(), sprintf("%s_source.R", x$id))
#   xx = base64Encode(rawToChar(serialize(x, connection = NULL, ascii = TRUE)))
#   writeLines(sprintf("
# sourcedFlow = function(task.id) {
#   library(RCurl)
#   library(mlr)
#   task = getOMLTask(task.id)
#   x = unserialize(charToRaw(base64Decode('%s')))
#   runTaskMlr(task, x)
# }", xx), sourcefile)
#   return(sourcefile)
# }

checkOMLFlow = function(x, verbosity = NULL){
  if (inherits(x, "Learner")) x = convertMlrLearnerToOMLFlow(x)

  content = doAPICall(api.call = paste0("flow/exists/", x$name, "/", x$external.version),
    method = "GET", file = NULL, verbosity = verbosity)

  doc = parseXMLResponse(content, "Checking existence of flow", "flow_exists", as.text = TRUE)

  return(
    list(
      exists = as.logical(xmlRValS(doc, "/oml:flow_exists/oml:exists")),
      doc = doc
    )
  )
}
