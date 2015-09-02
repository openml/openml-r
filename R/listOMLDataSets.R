#' @title List available OpenML data sets.
#' @description
#' The returned \code{data.frame} contains the data set id \dQuote{did},
#' the \dQuote{status} (\dQuote{active}, \dQuote{deactivated}, \dQuote{in_preparation})
#' and describing data qualities.
#' @template arg_verbosity
#' @template arg_status
#' @return [\code{data.frame}].
#' @family list
#' @export
listOMLDataSets = function(verbosity = NULL, status = "active") {
  url = getAPIURL("data/list")
  content = downloadXML(url, NULL, verbosity)
  xml = parseXMLResponse(content, "Getting data set list", "data", as.text = TRUE)
  status.levels = c("active", "deactivated", "in_preparation")
  assertSubset(status, status.levels)

  # get list of blocks for data sets
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  df = as.data.frame(rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    is.quality = names(children) == "quality"
    info = list(
      did = as.integer(xmlValue(children[["did"]])),
      status = xmlValue(children[["status"]]),
      name = xmlValue(children[["name"]])
    )
    qualities = convertNodeSetToList(children[is.quality], fun = as.integer)
    c(info, qualities)
  }), fill = TRUE))
  df$status = factor(df$status, levels = status.levels)

  # subset status level
  ret = droplevels(df[df$status%in%status, ])
  row.names(ret) = NULL
  return(ret)
}
