#' @title List available OpenML qualities names.
#'
#' @description
#' The returned \code{data.frame} contains quality name \dQuote{name}.
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family list
#' @export
#'
listOMLDataSetQualitiesList = function(verbosity = NULL) {
  url = getAPIURL("data/qualities/list")
  content = downloadXML(url, NULL, verbosity)
  doc = parseXMLResponse(content, "Getting names of available data set qualities", "data_qualities_list",
                         as.text = TRUE)
  data.frame(name = xmlValsMultNsS(doc, "/oml:data_qualities_list/oml:quality"), stringsAsFactors = FALSE)
}
