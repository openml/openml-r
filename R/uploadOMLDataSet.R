#' @title Upload a dataset to the OpenML server.
#'
#' @description
#' Share a dataset by uploading it to the OpenML server.
#'
#' @param x [\code{\link[mlr]{Task}}|[\code{\link{OMLDataSet}}]\cr
#'   Contains the dataset that should be uploaded.
#' @template arg_verbosity
#' @return [\code{invisible(numeric(1))}].
#'   The id of the data (\code{did}).
#' @family uploading functions
#' @family dataset related functions
#' @export
uploadOMLDataSet = function(x, verbosity = NULL) {
  UseMethod("uploadOMLDataSet")
}

#' @export
uploadOMLDataSet.OMLDataSet = function(x, verbosity = NULL) {
  description = tempfile()
  on.exit(unlink(description))
  writeOMLDataSetXML(x$desc, description)

  output = tempfile()
  on.exit(unlink(output), add = TRUE)
  if (getOMLConfig()$arff.reader == "RWeka")
    RWeka::write.arff(x$data, file = output)
  else
    farff::writeARFF(x$data, path = output)

  showInfo(verbosity, "Uploading data set to server.")

  response = doAPICall(api.call = "data", method = "POST", file = NULL, verbosity = verbosity,
    post.args = list(description = upload_file(path = description),
                     dataset = upload_file(path = output)))
  doc = parseXMLResponse(response, "Uploading dataset", c("upload_data_set", "response"), as.text = TRUE)
  did = xmlOValI(doc, "/oml:upload_data_set/oml:id")
  showInfo(verbosity, "Data set successfully uploaded. Data set ID: %i", did)
  return(invisible(did))
}

#' @export
uploadOMLDataSet.Task = function(x, verbosity = NULL) {
  x = createOMLDataSetFromMlrTask(x)
  uploadOMLDataSet.OMLDataSet(x)
}

createOMLDataSetFromMlrTask = function(task){
  desc = makeOMLDataSetDescription(id = 1L,
    name = task$task.desc$id,
    version = "1",
    description = task$task.desc$id,
    format = "ARFF",
    upload.date = as.POSIXct(Sys.time()),
    default.target.attribute = task$task.desc$target,
    status = "active"
  )

  oml.data = makeOMLDataSet(desc = desc,
    data = mlr::getTaskData(task),
    colnames.old = mlr::getTaskFeatureNames(task),
    colnames.new = mlr::getTaskFeatureNames(task),
    target.features = mlr::getTaskTargetNames(task)
  )
  return(oml.data)
}
