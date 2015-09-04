#' List available OpenML task types
#' @title List available OpenML task types.
#'
#' @description
#' The returned \code{data.frame} contains the type \code{id} and the character
#' name of the OpenML task type.
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
listOMLTaskTypes = function(verbosity = NULL) {
  content = doAPICall(api.call = "tasktype/list", file = NULL, verbosity = verbosity, method = "GET")
  xml = parseXMLResponse(content, "Getting task type list", "task_types", as.text = TRUE)
  data.frame(
    id = xmlValsMultNsI(xml, "/oml:task_types/oml:task_type/oml:id"),
    name = xmlValsMultNsS(xml, "/oml:task_types/oml:task_type/oml:name"),
    stringsAsFactors = FALSE
  )
}
