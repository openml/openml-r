#' OpenMLData
#'
#' This class of objects contains information on datasets and/or evaluations. This class is used in 
#' \code{\link{OpenMLRun}s}.
#' 
#' Objects can be created by calls of the form \code{OpenMLData(...)}.
#' The objects contain information on ... .
#'
#' @section Slots: 
#'  \describe{
#'    \item{\code{dataset}}{[\code{list}]\cr
#'    A list of short data set descriptions with elements "did", "name" and "url". This list is possibly empty.}
#'    \item{\code{evaluation}}{[\code{list}]\cr
#'    A list of evaluations. This list is possibly empty.}
#'  }
#' @name OpenMLData
#' @rdname OpenMLData
#' @aliases OpenMLData-class
#' @exportClass OpenMLData

setClass("OpenMLData", representation(
  dataset = "list",
  evaluation = "list"
))

# ***** Constructor *****
OpenMLData = function(dataset = list(), evaluation = list()) {
  makeS3Obj("OpenMLData", 
    dataset = dataset,
    evaluation = evaluation
  )
}

# ***** Methods *****

# show
#' @export
print.OpenMLData = function(object, ...)  function(object) {
  catf('\n** Data Set(s) **')
  print(do.call(rbind.data.frame, object$dataset))
  
  catf('\n** Evaluations **')
  print(do.call(rbind.data.frame, object$evaluation))
}
