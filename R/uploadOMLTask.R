#' @title Upload a task to the OpenML server.
#'
#' @description
#' Share a task by uploading it to the OpenML server.
#'
#' @export
uploadOMLTask = function(task_type_id, source_data, target_feature, estimation_procedure,
                         evaluation_measures = NULL, tags = NULL, description = NULL,
                         confirm.upload = NULL, verbosity = NULL) {
  
  desc.file = tempfile(fileext = ".xml")
  on.exit(unlink(desc.file))
  writeOMLTaskXML(task_type_id, source_data, target_feature, estimation_procedure,
                  desc.file, evaluation_measures)
  
  showInfo(verbosity, "Uploading data set to server.")
  
  res = doAPICall(api.call = "task", method = "POST", file = NULL, verbosity = verbosity,
                     post.args = list(description = upload_file(path = desc.file)))
  
  #doc = parseXMLResponse(response, "Uploading dataset", c("upload_data_set", "response"), as.text = TRUE)
  #task.id = xmlOValI(doc, "/oml:upload_data_set/oml:id")
  #showInfo(verbosity, "Data set successfully uploaded. Data set ID: %i", data.id)
  #if (!is.null(tags)) tagOMLObject(data.id, object = "data", tags = tags)
  #forget(listOMLDataSets)
  #return(invisible(data.id))
}
