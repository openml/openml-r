#' @title Upload a task to the OpenML server.
#'
#' @description
#' Share a task by uploading it to the OpenML server.
#'
#' @param task.type [character]\cr
#'   The type of the task to upload. See listOMLTask() to list all valid task types.
#' @param source.data [integer]\cr
#'   The ID of the data set on which the task should be executed.
#' @param target.feature [character]\cr
#'   The target feature of the dataset.
#' @param estimation.procedure [integer]\cr
#'   The estimation procedure for the evaluation. See listOMLEstimationProcedures() to list all procedures.
#' @template arg_upload_tags
#' @template arg_description
#' @template arg_confirm.upload
#' @template arg_verbosity
#' @export
uploadOMLTask = function(task.type, source.data, target.feature, estimation.procedure,
                         evaluation.measures = NULL, tags = NULL, description = NULL,
                         confirm.upload = NULL, verbosity = NULL) {
  assertCharacter(task.type)
  assertInt(source.data)
  assertCharacter(target.feature)
  assertInt(estimation.procedure)

  # check if task type exists and get the corresponding task.type.id
  task.types = listOMLTaskTypes()
  if (task.type %in% task.types$name) {
    task.type.id = task.types[task.types$name == task.type, ]$id
  } else {
    stopf("Unknown task.type: %s. Please use listOMLTaskTypes() to list all possible task types.", task.type)
  }

  # check if the estimation.procedure exists for the given task.type
  est.procedures = listOMLEstimationProcedures()
  if (!(est.procedures[est.procedures$est.id == estimation.procedure, ]$task.type == task.type)) {
    stopf("Invalid estimation.procedure: %d for the given task.type: %s.
          Please use listOMLEstimationProcedures() to list all possible estimation procedures.",
          estimation.procedure, task.type)
  }

  desc.file = tempfile(fileext = ".xml")
  on.exit(unlink(desc.file))

  writeOMLTaskXML(task.type.id, source.data, target.feature, estimation.procedure,
                  desc.file, evaluation.measures)
  showInfo(verbosity, "Uploading task to server.")
  response = doAPICall(api.call = "task", method = "POST", file = NULL, verbosity = verbosity,
                 post.args = list(description = upload_file(path = desc.file)))

  # extract task ID from response
  doc = parseXMLResponse(response, "Uploading task", c("upload_task", "response"), as.text = TRUE)
  task.id = xmlOValI(doc, "/oml:upload_task/oml:id")
  showInfo(verbosity, "Task successfully uploaded. Task ID: %i", task.id)

  return(invisible(task.id))
}
