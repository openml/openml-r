#' @title Convert \code{OMLMlrRun}s to a \code{BenchmarkResult}.
#'
#' @description
#' Converts one or more \code{\link{OMLMlrRun}}s to a \code{\link[mlr]{BenchmarkResult}}.
#'
#' @param ... [\code{\link{OMLMlrRun}}]\cr
#'   One or more \code{\link{OMLMlrRun}s}
#' @return [\code{\link[mlr]{BenchmarkResult}}].
#' @family run-related functions
#' @export
convertOMLMlrRunToBMR = function(...) {
  set = list(...)
  assertList(set, types = "OMLMlrRun")
  bmr = lapply(set, function(x) x$bmr)
  return(mlr::mergeBenchmarkResults(bmrs = bmr))
}
