#' @title Upload a task to the OpenML server.
#'
#' @description
#' Share a task by uploading it to the OpenML server.
#'
#' @param task.type [character(1)]\cr
#'   The type of the task to upload. See listOMLTaskTypes() to list all valid task types.
#' @template arg_data.id
#' @param target.feature [character(1)]\cr
#'   The target feature of the dataset.
#' @param estimation.procedure [character(1)]\cr
#'   The estimation procedure for the evaluation. See listOMLEstimationProcedures() to list all procedures.
#' @param evaluation.measure [character(1)]\cr
#'   The evaluation measure for the evaluation. See listOMLEvaluationMeasures() to list all possible measures.
#' @template arg_upload_tags
#' @template arg_description
#' @template arg_confirm.upload
#' @template arg_verbosity
#' @export
uploadOMLTask = function(task.type, data.id, target.feature, estimation.procedure,
  evaluation.measure = NULL, tags = NULL, description = NULL,
  confirm.upload = NULL, verbosity = NULL) {
  assertIntegerish(data.id)
  assertCharacter(target.feature)
  # check if task type is valid
  tt = listOMLTaskTypes()
  assertChoice(task.type, choices = tt$name)
  # check if the estimation.procedure of the given task.type is valid
  ep.df = listOMLEstimationProcedures()
  ep.df = ep.df[ep.df$task.type == task.type, ]
  assertChoice(estimation.procedure, choices = ep.df$name)
  assertChoice(evaluation.measure, choices = listOMLEvaluationMeasures()$name, null.ok = TRUE)

  if (!checkUserConfirmation(type = "task", confirm.upload = confirm.upload)) {
    return(invisible())
  }

  desc.file = tempfile(fileext = ".xml")
  on.exit(unlink(desc.file))

  task.type.id = tt[tt$name == task.type, "id"]
  est.id = ep.df[ep.df$name == estimation.procedure, "est.id"]

  writeOMLTaskXML(task.type.id, data.id, target.feature, est.id,
    desc.file, evaluation.measure)
  showInfo(verbosity, "Uploading task to server.")
  response = doAPICall(api.call = "task", method = "POST", file = NULL, verbosity = verbosity,
    post.args = list(description = upload_file(path = desc.file)))

  # extract task ID from response
  doc = parseXMLResponse(response, "Uploading task", c("upload_task", "response"), as.text = TRUE)
  task.id = xmlOValI(doc, "/oml:upload_task/oml:id")
  showInfo(verbosity, "Task successfully uploaded. Task ID: %i", task.id)

  if (!is.null(tags)) tagOMLObject(task.id, object = "task", tags = tags)

  return(invisible(task.id))
}
