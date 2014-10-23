#' @title Get table of available tasks OpenML server.
#'
#' @description
#' The returned data.frame contains the \code{task_id}, the data set id \code{did},
#' the \code{status} and some describing data qualities.
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
getOpenMLTaskTypeList = function(verbosity = NULL) {
  url = getAPIURL("openml.task.type")
  contents = downloadXML(url, NULL, verbosity)
  xml = parseXMLResponse(contents, "Getting task type list", "task_types", as.text = TRUE)
  d = data.frame(
    id = xmlValsMultNsI(xml, "/oml:task_types/oml:task_type/oml:id"),
    name = xmlValsMultNsS(xml, "/oml:task_types/oml:task_type/oml:name"),
    description = xmlValsMultNsS(xml, "/oml:task_types/oml:task_type/oml:description")
  )
  d = convertRowsToList(d, name.vector = TRUE, name.list = FALSE)
  setNames(d, extractSubList(d, "id"))
}


