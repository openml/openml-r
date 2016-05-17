.listOMLFlows = function(tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {
  api.call = generateAPICall("flow/list", tag = tag, limit = limit, offset = offset)
  
  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")

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
#' @template arg_tag
#' @template arg_limit
#' @template arg_offset
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @family flow-related functions
#' @export
#' @example inst/examples/listOMLFlows.R
listOMLFlows = memoise(.listOMLFlows)
