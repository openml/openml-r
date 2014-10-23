#' Print the description of an OpenML object.
#'
#' @param x
#'   The object whose description should be printed. This is typically an \code{\link{OMLDataSet}},
#'   an \code{\link{OMLDataSetDescription}}, an \code{\link{OpenMLImplementation}} or an
#'   \code{\link{OpenMLTask}}. In the latter case, the description of the corresponding
#'   data set is printed.
#' @export
printDescription = function(x) {
  UseMethod("printDescription")
}

#' @export
printDescription.OpenMLTask = function(x) {
  cat(x$data.set$desc$description)
}

#' @export
printDescription.OMLDataSetDescription = function(x) {
  cat(x$description)
}

#' @export
printDescription.OMLDataSet = function(x) {
  cat(x$desc$description)
}

#' @export
printDescription.OpenMLImplementation = function(x) {
  cat(x$description)
}
