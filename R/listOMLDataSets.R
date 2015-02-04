#' @title List available OpenML data sets.
#' @description
#' The returned \code{data.frame} contains the data set id \dQuote{did},
#' the \dQuote{status} (\dQuote{active}, \dQuote{inactive}, \dQuote{in_preparation})
#' and describing data qualities.
#' @template arg_hash
#' @template arg_verbosity
#' @template arg_status
#' @return [\code{data.frame}].
#' @family list
#' @export
listOMLDataSets = function(session.hash = getSessionHash(), 
    verbosity = NULL, status = "active") {
  url = getAPIURL("openml.data")
  content = downloadXML(url, NULL, verbosity, session_hash = session.hash)
  xml = parseXMLResponse(content, "Getting data set list", "data", as.text = TRUE)
  status.levels = c("active", "deactivated", "in_preparation")
  assertSubset(status, status.levels)
  assertString(session.hash)
  
  # get list of blocks for data sets
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  df = as.data.frame(rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    is.quality = names(children) == "quality"
    info = list(did = as.integer(xmlValue(children[["did"]])), status = xmlValue(children[["status"]]))
    qualities = convertNodeSetToList(children[is.quality], fun = as.integer)
    c(info, qualities)
  }), fill = TRUE))
  df$status = factor(df$status, levels = status.levels)
  
  # subset status level
  ret = droplevels(df[df$status%in%status, ])
  row.names(ret) = NULL
  return(ret)
}
