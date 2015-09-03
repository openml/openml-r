#' @title List all registered OpenML flows.
#'
#' @description
#' The returned \code{data.frame} contains the flow id \dQuote{fid},
#' the flow name (\dQuote{full.name} and \dQuote{name}), version information
#' (\dQuote{version} and \dQuote{external.version}) and the uploader (\dQuote{uploader})
#' of all registered OpenML flows.
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family list
#' @export
listOMLFlows = function(verbosity = NULL) {
  url = getAPIURL("flow/list")
  content = doAPICallGET(url, NULL, verbosity = verbosity)
  xml = parseXMLResponse(content, "Getting flows", "implementations", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  as.data.frame(rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    list(
      implementation.id = as.integer(xmlValue(children[["id"]])),
      full.name = xmlValue(children[["full_name"]]),
      name = xmlValue(children[["name"]]),
      version = as.integer(xmlValue(children[["version"]])),
      external.version = xmlValue(children[["external_version"]]),
      uploader = as.integer(xmlValue(children[["uploader"]]))
    )
  }), fill = TRUE))
}
