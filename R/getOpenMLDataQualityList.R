#' Get all names of OpenML data qualities.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console? Default is \code{FALSE}.
getOpenMLDataQualityList = function(show.info = FALSE) {
  file = tempfile()
  on.exit({
    unlink(file)
  })
  downloadAPICallFile(api.fun = "openml.data.qualities.list", file = file, show.info = show.info)
  doc = parseXMLResponse(file, "Getting names of data qualities", "data_qualities_list")
  dql = xmlValsMultNsS(doc, "/oml:data_qualities_list/oml:quality")
  return(dql)
}