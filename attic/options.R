#' @title Returns a list of OpenML's options
#'
#' @return [\code{list}].
#' @export
#' @family configure
getOpenMLOptions = function() {
  oml.inds = substr(names(options()), start = 1L, stop = 7L) == "OpenML."
  oml.options = options()[oml.inds]
  names(oml.options) = substring(names(oml.options), first = 8L)
  return(oml.options)
}

setOpenMLOption = function(name, val) {
  name = sprintf("OpenML.%s", name)
  val = list(val)
  names(val) = name
  do.call(options, val)
}

getOpenMLOption = function(name, default) {
  getOption(sprintf("OpenML.%s", name), default)
}
