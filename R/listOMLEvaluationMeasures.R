#' @title List available OpenML evaluation measures.
#' @description
#' The names of all evaluation measures which are used in at least one run are returned
#' in a \code{data.frame}.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family list
#' @export
listOMLEvaluationMeasures = function(session.hash = getSessionHash(), verbosity = NULL) {
  url = getAPIURL("openml.evaluation.measures")
  content = downloadXML(url, NULL, verbosity, session_hash = session.hash)
  doc = parseXMLResponse(content, "Getting names of evaluation measures", "evaluation_measures",
    as.text = TRUE)
  data.frame(name = xmlValsMultNsS(doc, "/oml:evaluation_measures/oml:measures/oml:measure"))
}
