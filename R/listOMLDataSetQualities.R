.listOMLDataSetQualities = function(verbosity = NULL) {
  content = doAPICall(
    api.call = "json/data/qualities/list", file = NULL,
    verbosity = verbosity, method = "GET"
  )
  data.frame(
    name = fromJSON(txt = content)$data_qualities_list$quality,
    stringsAsFactors = FALSE
  )
}

#' @title List available OpenML qualities names.
#'
#' @description
#' The returned \code{data.frame} contains quality name \dQuote{name}.
#'
#' @template note_memoise
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
#' @example inst/examples/listOMLDataSetQualities.R
listOMLDataSetQualities = memoise(.listOMLDataSetQualities)
