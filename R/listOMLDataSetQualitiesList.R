#' @title List available OpenML qualities names.
#'
#' @description
#' The returned \code{data.frame} contains quality name \dQuote{name}.
#'
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family list
#' @export
listOMLDataSetQualitiesList = function(session.hash = getSessionHash(),
    verbosity = NULL) {
  assertString(session.hash)
  url = getAPIURL("openml.data.qualities.list")
  content = downloadXML(url, NULL, verbosity, session_hash = session.hash)
  doc = parseXMLResponse(content, "Getting names of available data set qualities", "data_qualities_list",
                         as.text = TRUE)
  data.frame(name = xmlValsMultNsS(doc, "/oml:data_qualities_list/oml:quality"), stringsAsFactors = FALSE)
}
