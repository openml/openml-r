#' @title Get information about all tasks registred on OpenML server.
#'
#' @description
#' We currently return only  which are marked as safe on OpenML.
#'
#'
#' @return [\code{data.frame}].
#' @export
#FIXME: document better

getOpenMLRegisteredTasks = function(type = "classification") {
  assertSubset(type, "classification")
  file = tempfile()
  downloadAPICallFile(api.fun = "openml.task.classification.safe", file = file, show.info = FALSE)
  doc = parseXMLResponse(file, "Getting safe task info", "task-classification-safe")
  d = data.frame(
    task_id = xmlValsMultNsI(doc, path = "/oml:task-classification-safe/oml:task/oml:task_id"),
    data_name = xmlValsMultNsS(doc, path = "/oml:task-classification-safe/oml:task/oml:data_name"),
    data_version = xmlValsMultNsI(doc, path = "/oml:task-classification-safe/oml:task/oml:data_version"),
    stringsAsFactors = FALSE
  )

  return(d)
}
