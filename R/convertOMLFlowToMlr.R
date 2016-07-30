#' @title Converts an flow to a mlr learner.
#'
#' @description
#' Converts an \code{\link{OMLFlow}} that was originally created with the OpenML 
#' R-package to a \code{\link[mlr]{Learner}}.
#'
#' @param flow [\code{\link{OMLFlow}}]\cr
#'   The flow object.
#'
#' @return [\code{\link[mlr]{Learner}}].
#' @family flow-related functions
#' @export
convertOMLFlowToMlr = function(flow) {
  if (grepl("^R_", flow$external.version)) {
    if (grepl("-v2[[:punct:]]", flow$external.version)) {
      assertFile(flow$binary.path)
      lrn = readRDS(flow$binary.path)
    } else {
      lrn = makeLearner(flow$name)
    }
    return(lrn)
  } else {
   stopf("This flow can not be converted to a mlr learner.") 
  }
}
