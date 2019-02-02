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
  showInfo(verbosity, "Uploading task to server.")
  res = tryCatch(
    {
      response = doAPICall(api.call = "task", method = "POST", file = NULL, verbosity = verbosity,
                     post.args = list(description = upload_file(path = desc.file)))
      doc = parseXMLResponse(response, "Uploading task", c("upload_task", "response"), as.text = TRUE)
      task.id = xmlOValI(doc, "/oml:upload_task/oml:id")
      showInfo(verbosity, "Task successfully uploaded. Task ID: %i", task.id)
      return(invisible(task.id))
    },
    error = function(err) {
      print(paste0(conditionMessage(err), " for data.id = ", source_data))
      return(invisible(NA))
    }
  )
  return(res)
}
