#' @title List all registered OpenML flows.
#'
#' @description The returned data.frame contains some describing information like \code{id},
#' \code{name} and \code{version} on all registered OpenML flows.
#'
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
listOMLFlows = function(session.hash = getSessionHash(), verbosity = NULL) {
  url = getAPIURL("openml.implementations")
  content = downloadXML(url, NULL, verbosity = verbosity, session_hash = session.hash)
  xml = parseXMLResponse(content, "Getting flows", "implementations", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  flows = rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    qualities = names(children) == "implementation"
    row = c(
      list(
        as.integer(xmlValue(children[["id"]])),
        xmlValue(children[["full_name"]]),
        xmlValue(children[["name"]]),
        as.integer(xmlValue(children[["version"]])),
        xmlValue(children[["external_version"]]),
        as.integer(xmlValue(children[["uploader"]]))
      ),
      as.list(as.integer(vcapply(children[qualities], xmlValue)))
    )
    names(row) = c("id", "full.name", "name", "version", "external.version", "uploader")
    row
  }), fill = TRUE)

  as.data.frame(flows)
}
