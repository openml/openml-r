#' @title List available OpenML data sets
#'
#' @description
#' The returned data.frame contains the data set id \code{did}, the \code{status} and
#' some describing data qualities.
#' @template arg_hash
#'
#' @template arg_verbosity
#' @return data.frame
#' @export
listOMLDataSets = function(session.hash = getSessionHash(), verbosity = NULL) {
  url = getAPIURL("openml.data")
  content = downloadXML(url, NULL, verbosity, session_hash = session.hash)
  xml = parseXMLResponse(content, "Getting data set list", "data", as.text = TRUE)
  # get list of blocks for data sets
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  quals = rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    qualities = names(children) == "quality"
    row = c(
      list(as.integer(xmlValue(children[["did"]]))), status = xmlValue(children[["status"]]),
      as.list(as.integer(vcapply(children[qualities], xmlValue)))
    )
    names(row) = c("did", "status", vcapply(children[qualities], xmlAttrs))
    row
  }), fill = TRUE)
  df = as.data.frame(quals)
  df$status = factor(df$status, levels = c("active", "inactive", "in_preparation"))
  return(df)
}
