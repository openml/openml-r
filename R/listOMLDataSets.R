#' @title List available OpenML data sets.
#'
#' @description
#' The returned \code{data.frame} contains the data set id \dQuote{did},
#' the \dQuote{status} (\dQuote{active}, \dQuote{deactivated}, \dQuote{in_preparation})
#' and describing data qualities.
#'
#' @template arg_verbosity
#' @template arg_status
#' @return [\code{data.frame}].
#' @family listing functions
#' @family dataset related functions
#' @export
listOMLDataSets = function(verbosity = NULL, status = "active") {
  content = doAPICall(api.call = "data/list", file = NULL, verbosity = verbosity, method = "GET")
  xml = parseXMLResponse(content, "Getting data set list", "data", as.text = TRUE)
  assertSubset(status, getValidOMLDataSetStatusLevels())

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
  df$status = factor(df$status, levels = getValidOMLDataSetStatusLevels())

  # subset status level
  ret = droplevels(df[df$status%in%status, , drop = FALSE])
  row.names(ret) = NULL
  return(ret)
}
