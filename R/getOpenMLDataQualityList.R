#' Get all names of OpenML data qualities.
#' @template arg_showinfo
getOpenMLDataQualityList = function(show.info = getOpenMLOption("show.info")) {
  url = getAPIURL("openml.data.qualities.list")
  doc = parseXMLResponse(url, "Getting names of data qualities", "data_qualities_list")
  xmlValsMultNsS(doc, "/oml:data_qualities_list/oml:quality")
}
