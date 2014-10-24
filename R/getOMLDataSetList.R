#' @title Get table of available data sets on OpenML server.
#'
#' @description
#' The returned data.frame contains the data set id \code{did}, the \code{status} and
#' some describing data qualities.
#'
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
getOMLDataSetList = function(session.hash, verbosity = NULL) {
  url = getAPIURL("openml.data")
  content = postFormOML(url, NULL, verbosity, session_hash = session.hash)
  xml = parseXMLResponse(content, "Getting data set list", "data", as.text = TRUE)
  # get list of blocks for data sets
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  quals = list()
  for (i in seq_along(blocks)) {
    node = blocks[[i]]
    quals1 = xmlChildren(node)[-(1:2)]
    quals2 = as.list(as.numeric(vcapply(quals1, xmlValue)))
    names(quals2) = vcapply(quals1, xmlAttrs)
    # make sure that empty row without qualities are not dropped by rbind.fill
    quals2$.foo = 1
    quals[[i]] = as.data.frame(quals2)
  }
  quals = do.call(rbind.fill, quals)
  quals = cbind(
    did = xmlValsMultNsI(xml, "/oml:data/oml:dataset/oml:did"),
    status = xmlValsMultNsS(xml, "/oml:data/oml:dataset/oml:status"),
    quals
  )
  quals$.foo = NULL
  return(quals)
}
