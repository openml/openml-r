#' @title Returns a list of OpenML configuration settings
#'
#' @return \code{list} of current configuration variables with class \dQuote{OMLConfig}.
#' @family config
#' @export
getOMLConfig = function() {
  return(.OpenML.config)
}
