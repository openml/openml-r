#' OpenMLImplementationParameter
#'
#' This class of objects contains information on an implementation parameter.
#' 
#' Objects can be created by calls of the form \code{OpenMLImplementationParameter(...)}.
#' The objects contain information on ... .
#'
#' @section Slots: 
#'  \describe{
#'    \item{\code{name}}{[\code{character}]\cr
#'    The name of the parameter.}
#'    \item{\code{data.type}}{[\code{character}]\cr
#'    The data type of the parameter. Should be either integer, numeric, string, vector, matrix, object.}
#'    \item{\code{default.value}}{[\code{character}]\cr
#'    The default value of the parameter. Optional, but highly encouraged.}
#'    \item{\code{description}}{[\code{character}]\cr
#'    A description of what this parameter does.}
#'  }
#' @name OpenMLImplementationParameter
#' @rdname OpenMLImplementationParameter
#' @aliases OpenMLImplementationParameter-class
#' @exportClass OpenMLImplementationParameter

setClass("OpenMLImplementationParameter", representation(
  name = "character",
  data.type = "character",
  default.value = "character",
  description = "character"
))

# ***** Constructor *****
OpenMLImplementationParameter = function(
  name, 
  data.type = character(0), 
  default.value = character(0), 
  description = character(0)) {
  
  makeS3Obj("OpenMLImplementationParameter", 
      name = name,
      data.type = data.type,
      default.value = default.value,
      description = description
  )
}

# ***** Methods *****

# show
# FIXME: how should missing values be represented? here, character(0) AND "" are possible.
#' @export
print.OpenMLImplementationParameter = function(x, ...) {  
  catf("Parameter %s", x$name)  
  if (length(x$data.type) > 0 && x$data.type != "")
    catf("  type    :: %s", x$data.type)
  if (length(x$default.value) > 0 && x$default.value != "")
    catf("  default :: %s", x$default.value)
  if (length(x$description) > 0 && x$description != "")
    catf("\n%s", x$description)
}
