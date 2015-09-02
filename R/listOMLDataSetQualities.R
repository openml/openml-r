#' @title List available OpenML qualities with values for given data set.
#'
#' @description
#' The returned \code{data.frame} contains data set quality \dQuote{name} and value \dQuote{value}.
#'
#' @param did [\code{integer(1)}]\cr
#'   The data set ID
#' @template arg_verbosity
#' @param name [\code{character}]\cr
#'   Retruns only the data qualities from \dQuote{name} (see also \code{\link{listOMLDataSetQualitiesList}}).
#'   Default is \code{NULL} and uses all available data qualities.
#' @return [\code{data.frame}].
#' @family list
#' @export
#'
listOMLDataSetQualities = function(did, verbosity = NULL, name = NULL) {
#   qualities = listOMLDataSetQualitiesList()$name
#   if(is.null(name)) name = qualities
#   assertSubset(name, qualities)
  
  url = getAPIURL("data/qualities/", get.arg = did)
  content = downloadXML(url, NULL, verbosity = verbosity, post = FALSE)
  xml = parseXMLResponse(content, "Getting data set qualities", "data_qualities", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  ret = as.data.frame(rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    list(
      name = xmlValue(children[["name"]]),
      value = as.numeric(xmlValue(children[["value"]]))
    )
  }), fill = TRUE))
  
  if(is.null(name)) return(ret) 
  if(any(ret$name%in%name)) return(ret[ret$name%in%name, ]) else 
    stop("Data quality in 'name' not found.")
}
