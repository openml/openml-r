#' @title List available OpenML qualities with values for given data set.
#'
#' @description The returned \code{data.frame} contains data set quality
#' \dQuote{name} and value \dQuote{value}.
#'
#' @param did [\code{integer(1)}]\cr
#'   The data set ID.
#' @template arg_verbosity
#' @param name [\code{character}]\cr
#'   Returns only the data qualities from \dQuote{name} (see also \code{\link{listOMLDataSetQualities}}).
#'   Default is \code{NULL} and uses all available data qualities.
#' @return [\code{data.frame}].
#' @family downloading functions
#' @example inst/examples/getOMLDataSetQualities.R
#' @export
getOMLDataSetQualities = function(did, verbosity = NULL, name = NULL) {
  qualities = listOMLDataSetQualities()$name
  if (is.null(name))
    name = qualities
  assertSubset(name, qualities)

  content = doAPICall(
    api.call = "data/qualities", id = did,
    file = NULL, verbosity = verbosity, method = "GET"
  )
  xml = parseXMLResponse(content, "Getting data set qualities", "data_qualities", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  ret = as.data.frame(rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    list(
      name = xmlValue(children[["name"]]),
      value = as.numeric(xmlValue(children[["value"]]))
    )
  }), fill = TRUE))

  if (is.null(name))
    return(ret)
  if (any(ret$name %in% name))
    return(ret[ret$name %in% name, , drop = FALSE])
  stop("Data quality in 'name' not found.")
}
