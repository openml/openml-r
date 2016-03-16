.listOMLFlows = function(verbosity = NULL, tag = NULL) {
  api.call = "flow/list"
  if (!is.null(tag)) {
    assertString(tag, na.ok = FALSE)
    api.call = collapse(c(api.call, "tag", tag), sep = "/")
  }
  content = try(doAPICall(api.call = "flow/list", file = NULL, verbosity = verbosity, method = "GET"))
  if (is.error(content)) {
    return(data.frame())
  }

  xml = parseXMLResponse(content, "Getting flows", "flows", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  as.data.frame(rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    list(
      flow.id = as.integer(xmlValue(children[["id"]])),
      full.name = xmlValue(children[["full_name"]]),
      name = xmlValue(children[["name"]]),
      version = as.integer(xmlValue(children[["version"]])),
      external.version = xmlValue(children[["external_version"]]),
      uploader = as.integer(xmlValue(children[["uploader"]]))
    )
  }), fill = TRUE))
}

#' @title List all registered OpenML flows.
#'
#' @description
#' The returned \code{data.frame} contains the flow id \dQuote{fid},
#' the flow name (\dQuote{full.name} and \dQuote{name}), version information
#' (\dQuote{version} and \dQuote{external.version}) and the uploader (\dQuote{uploader})
#' of all registered OpenML flows.
#'
#' @template note_memoise
#'
#' @template arg_verbosity
#' @template arg_tag
#' @return [\code{data.frame}].
#' @family listing functions
#' @family flow related functions
#' @export
#' @example inst/examples/listOMLFlows.R
listOMLFlows = memoise(.listOMLFlows)
