#' @title Upload a dataset to the OpenML server.
#'
#' @description
#' Share a dataset by uploading it to the OpenML server.
#'
#' @note
#' This function will reset the cache of \code{link{listOMLDataSets}} on success.
#'
#' @param x [\code{\link[mlr]{Task}}|[\code{\link{OMLDataSet}}]\cr
#'   Contains the dataset that should be uploaded.
#' @template arg_description
#' @template arg_verbosity
#' @return [\code{invisible(numeric(1))}].
#'   The id of the data (\code{did}).
#' @family uploading functions
#' @family dataset related functions
#' @export
uploadOMLDataSet = function(x, description = NULL, verbosity = NULL) {
  UseMethod("uploadOMLDataSet")
}

#' @export
uploadOMLDataSet.OMLDataSet = function(x, description = NULL, verbosity = NULL) {
  if (!checkUserConfirmation(type = "dataset")) {
    return(invisible())
  }

  desc.file = tempfile()
  on.exit(unlink(desc.file))
  writeOMLDataSetXML(x$desc, desc.file)

  output = tempfile()
  on.exit(unlink(output), add = TRUE)
  if (getOMLConfig()$arff.reader == "RWeka")
    RWeka::write.arff(x$data, file = output)
  else
    farff::writeARFF(x$data, path = output)

  showInfo(verbosity, "Uploading data set to server.")

  response = doAPICall(api.call = "data", method = "POST", file = NULL, verbosity = verbosity,
    post.args = list(description = upload_file(path = desc.file),
                     dataset = upload_file(path = output)))
  doc = parseXMLResponse(response, "Uploading dataset", c("upload_data_set", "response"), as.text = TRUE)
  did = xmlOValI(doc, "/oml:upload_data_set/oml:id")
  showInfo(verbosity, "Data set successfully uploaded. Data set ID: %i", did)
  forget(listOMLDataSets)
  return(invisible(did))
}

#' @export
uploadOMLDataSet.Task = function(x, description = NULL, verbosity = NULL) {
  x = convertMlrTaskToOMLDataSet(x, description = description)
  uploadOMLDataSet.OMLDataSet(x)
}

#' @title Converts a mlr task to an OpenML data set.
#'
#' @description
#' Converts a \code{\link[mlr]{Task}} to an \code{\link{OMLDataSet}}.
#'
#' @param task [\code{\link[mlr]{Task}}]\cr
#'   A mlr task.
#' @template arg_description
#'
#' @return [\code{\link{OMLDataSet}}].
#' @family dataset related functions
#' @export
convertMlrTaskToOMLDataSet = function(task, description = NULL){
  assert(checkClass(description, "character"), checkClass(description, "OMLDataSetDescription"), checkNull(description))
  assertClass(task, "Task")

  if (is.null(description))
    description = as.character(task$task.desc$id)

  if (isTRUE(checkClass(description, "OMLDataSetDescription"))) {
    desc = description
  } else {
    desc = makeOMLDataSetDescription(
      name = task$task.desc$id,
      version = "1",
      description = description,
      format = "ARFF",
      upload.date = as.POSIXct(Sys.time()),
      default.target.attribute = task$task.desc$target,
      status = "active"
    )
  }

  cns = colnames(mlr::getTaskData(task))

  oml.data = makeOMLDataSet(desc = desc,
    data = mlr::getTaskData(task),
    colnames.old = cns,
    colnames.new = cns,
    target.features = mlr::getTaskTargetNames(task)
  )
  return(oml.data)
}
