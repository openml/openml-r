#' OpenMLBibRef
#'
#' This class of objects contains information on a bibliographical reference for an implementation.
#' 
#' Objects can be created by calls of the form \code{OpenMLBibRef(...)}.
#' The objects contain information on ... .
#'
#'$section Slots: 
#'  \describe{
#'    \item{\code{citation}}{[\code{character}]\cr
#'    Free form reference for this implementation.}
#'    \item{\code{url}}{[\code{character}]\cr
#'    URL to an online version of the paper, e.g. PDF.}
#'  }
#' @name OpenMLBibRef
#' @rdname OpenMLBibRef
#' @aliases OpenMLBibRef-class
#' @exportClass OpenMLBibRef

setClass("OpenMLBibRef", representation(
  citation = "character",
  url = "character"
))

# ***** Constructor *****
OpenMLBibRef = function(
  citation = character(0), 
  url = character(0)) {
  
  makeS3Obj("OpenMLBibRef", 
      citation = citation,
      url = url
  )
}

# ***** Methods *****

# show
# FIXME: how should missing values be represented? here, character(0) AND "" are possible.
setMethod("show", "OpenMLBibRef", function(object) {  
  catf("  %s", object$citation)  
  if(length(object$url) > 0 && object$url != "")
    catf("  url :: %s\n", object$url)
})
