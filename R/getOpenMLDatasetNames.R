#' @title Get all names of OpenML data sets which are registered on the server.
#'
#' @description
#' We currently return only data sets which are marked as safe on OpenML.
#'
#'
#' @return [\code{data.frame}].
#' @export
getOpenMLDatasetNames = function() {
  # FIXME: document better
  # FIXME: cleanup?
  file = tempfile()
  downloadAPICallFile(api.fun = "openml.data.safe", file = file, show.info = FALSE)
  doc = parseXMLResponse(file, "Getting safe data set names", "data-safe")
  data.frame(
    did = xmlValsMultNsI(doc, path = "/oml:data-safe/oml:data/oml:did"),
    name = xmlValsMultNsS(doc, path = "/oml:data-safe/oml:data/oml:name"),
    version = xmlValsMultNsI(doc, path = "/oml:data-safe/oml:data/oml:version"),
    stringsAsFactors = FALSE
  )
}
