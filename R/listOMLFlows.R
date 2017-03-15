.listOMLFlows = function(tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {
  api.call = generateAPICall("json/flow/list", tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")

  flows = fromJSON(txt = content)$flows$flow

  # type conversions
  flows$id = as.integer(flows$id)
  flows$version = as.integer(flows$version)
  flows$uploader = as.integer(flows$uploader)
  flows$tags = vcapply(flows$tags, function(x) collapse(x, ", "))

  # for some reason external_version is NOT atomic
  # Unfortunately unlist() drops character(0) entries!
  # -> ugly workaround: replace with "" -.-
  flows$external_version[lengths(flows$external_version) == 0L] = ""
  flows$external_version = unlist(flows$external_version)

  names(flows) = convertNamesOMLToR(names(flows))
  names(flows) = gsub("^id$", "flow.id", names(flows))
  return(flows)
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
