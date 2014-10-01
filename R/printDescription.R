#' Print the description of an OpenML object.
#' 
#' @param x
#'   The object whose description should be printed. This is typically an
#'   \code{\link{OpenMLDataSetDescription}}, an \code{\link{OpenMLImplementation}} or an 
#'   \code{\link{OpenMLTask}}. In the latter case, the description of the corresponding 
#'   data set is printed.
#' @export
printDescription = function(x) {
  UseMethod("printDescription")
}

#' @export
printDescription.OpenMLTask = function(x) {
  cat(x$data.desc$description)
}

#' @export
printDescription.OpenMLDataSetDescription = function(x) {
  cat(x$description)
}

#' @export
printDescription.OpenMLImplementation = function(x) {
  cat(x$description)
}