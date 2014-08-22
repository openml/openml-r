#' @title Get all names of OpenML data sets which are registered on the server.
#'
#' @description
#' We currently return only data sets which are marked as safe on OpenML.
#'
#'
#' @return [\code{data.frame}].

#FIXME: document better

getOpenMLDatasetNames = function(only.newest = TRUE) {
  file = tempfile()
  downloadAPICallFile(api.fun = "openml.data.safe", file = file, show.info = FALSE)
  doc = parseXMLResponse(file, "Getting safe data set names", "data-safe")
  d = data.frame(
    did = xmlValsMultNsI(doc, path = "/oml:data-safe/oml:data/oml:did"),
    name = xmlValsMultNsS(doc, path = "/oml:data-safe/oml:data/oml:name"),
    version = xmlValsMultNsI(doc, path = "/oml:data-safe/oml:data/oml:version")
  )

  # reduce to most recent version
  if (only.newest) {
    d = ddply(d, "name", function(d) {
      d[which.max(d$version), ]
    })
  }
  return(d)
}

