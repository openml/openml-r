#' Get all names of OpenML evaluation measures.
#' @template arg_hash
#' @template arg_verbosity
getOMLEvaluationMeasures = function(session.hash, verbosity = NULL) {
  url = getAPIURL("openml.evaluation.measures")
  content = postFormOML(url, NULL, verbosity, session_hash = session.hash)
  doc = parseXMLResponse(content, "Getting names of evaluation measures", "evaluation_measures",
    as.text = TRUE)
  xmlValsMultNsS(doc, "/oml:evaluation_measures/oml:measures/oml:measure")
}
