.listOMLFlows = function(tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {
  api.call = generateAPICall("json/flow/list", tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")

  flows = fromJSON(txt = content)$flows$flow

  # type conversions
  flows$id = as.integer(flows$id)
  flows$version = as.integer(flows$version)
  flows$uploader = as.integer(flows$uploader)

  # for some reason external_version is NOT atomic
  # Unfortunately unlist() drops character(0) entries!
  # -> ugly workaround: replace with "" -.-
  zero.len.ids = sapply(flows$external_version, function(x) identical(x, character(0)))
  flows$external_version[zero.len.ids] = ""
  flows$external_version = unlist(flows$external_version)

  names(flows) = c("id", "full.name", "name", "version", "external.version", "uploader")
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
