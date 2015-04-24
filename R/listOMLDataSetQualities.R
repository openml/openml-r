#' @title List available OpenML qualities with values for given data set.
#'
#' @description
#' The returned \code{data.frame} contains data set quality \dQuote{name} and value \dQuote{value}.
#'
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family list
#' @export
#'
listOMLDataSetQualities = function(data.id, session.hash = getSessionHash(),
    verbosity = NULL) {
  assertString(session.hash)
  url = getAPIURL("openml.data.qualities", data_id = data.id)
  content = downloadXML(url, NULL, verbosity = verbosity, session_hash = session.hash)
  xml = parseXMLResponse(content, "Getting data set qualities", "data_qualities", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  as.data.frame(rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    list(
      name = xmlValue(children[["name"]]),
      value = as.numeric(xmlValue(children[["value"]]))
    )
  }), fill = TRUE))
}
