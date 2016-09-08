.listOMLEvaluationMeasures = function(verbosity = NULL) {
  content = doAPICall(
    api.call = "json/evaluationmeasure/list", file = NULL,
    verbosity = verbosity, method = "GET"
  )
  res = fromJSON(txt = content)
  data.frame(
    name = res$evaluation_measures$measures$measure,
    stringsAsFactors = FALSE
  )
}

#' @title List available OpenML evaluation measures.
#'
#' @description
#' The names of all evaluation measures which are used in at least one run are returned
#' in a \code{data.frame}.
#'
#' @template note_memoise
#'
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @export
#' @example inst/examples/listOMLEvaluationMeasures.R
listOMLEvaluationMeasures = memoise(.listOMLEvaluationMeasures)
