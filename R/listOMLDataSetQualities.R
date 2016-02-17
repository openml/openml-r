.listOMLDataSetQualities = function(verbosity = NULL) {
  content = doAPICall(
    api.call = "data/qualities/list", file = NULL,
    verbosity = verbosity, method = "GET"
  )
  doc = parseXMLResponse(content, "Getting names of available data set qualities",
    "data_qualities_list", as.text = TRUE)
  data.frame(
    name = xmlValsMultNsS(doc, "/oml:data_qualities_list/oml:quality"),
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
listOMLDataSetQualities = memoise(.listOMLDataSetQualities, ~timeout(300L))
