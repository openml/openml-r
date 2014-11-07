#' Get all names of OpenML data qualities.
#' @template arg_hash
#' @template arg_verbosity
getOMLDataQualityList = function(session.hash, verbosity = NULL) {
  url = getAPIURL("openml.data.qualities.list")
  content = postFormOML(url, NULL, verbosity, session_hash = session.hash)
  doc = parseXMLResponse(content, "Getting names of data qualities", "data_qualities_list", 
    as.text = TRUE)
  xmlValsMultNsS(doc, "/oml:data_qualities_list/oml:quality")
}
