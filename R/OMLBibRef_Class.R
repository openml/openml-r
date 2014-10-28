#' @title Construct OMLBibRef.
#'
#' @param citation [\code{character(1)}]\cr
#'    Free form reference for this implementation.
#' @param url [\code{character(1)}]\cr
#'   URL to an online version of the paper, e.g. PDF.
#' @export
#' @aliases OMLBibRef
makeOMLBibRef = function(citation, url) {
  assertString(citation)
  assertString(url)
  makeS3Obj("OMLBibRef",
      citation = citation,
      url = url
  )
}

# ***** Methods *****

# show
# FIXME: how should missing values be represented? here, character(0) AND "" are possible.
#' @export
print.OMLBibRef = function(x, ...) {
  catf("  %s", x$citation)
  if (length(x$url) > 0L && nzchar(x$url))
    catf("  url :: %s\n", x$url)
}
