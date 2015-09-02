#' List available OpenML task types
#' @template arg_verbosity
#' @return [\code{data.frame}] with columns \dQuote{id} and \dQuote{name}.
#' @export
listOMLTaskTypes = function(verbosity = NULL) {
  url = getAPIURL("tasktype/list")
  content = downloadXML(url, NULL, verbosity)
  xml = parseXMLResponse(content, "Getting task type list", "task_types", as.text = TRUE)
  data.frame(
    id = xmlValsMultNsI(xml, "/oml:task_types/oml:task_type/oml:id"),
    name = xmlValsMultNsS(xml, "/oml:task_types/oml:task_type/oml:name"),
    stringsAsFactors = FALSE
  )
}
