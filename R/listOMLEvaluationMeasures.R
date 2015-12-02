#' @title List available OpenML evaluation measures.
#'
#' @description
#' The names of all evaluation measures which are used in at least one run are returned
#' in a \code{data.frame}.
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
listOMLEvaluationMeasures = function(verbosity = NULL) {
  content = doAPICall(api.call = "evaluationmeasure/list", file = NULL, verbosity = verbosity, method = "GET")
  doc = parseXMLResponse(content, "Getting names of evaluation measures", "evaluation_measures", as.text = TRUE)
  data.frame(name = xmlValsMultNsS(doc, "/oml:evaluation_measures/oml:measures/oml:measure"), stringsAsFactors = FALSE)
}
