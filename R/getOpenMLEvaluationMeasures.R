#' Get all names of OpenML evaluation measures.
#' @template arg_showinfo
getOpenMLEvaluationMeasures = function(show.info = getOpenMLOption("show.info")) {
  url = getAPIURL("openml.evaluation.measures")
  doc = parseXMLResponse(url, "Getting names of evaluation measures", "evaluation_measures")
  xmlValsMultNsS(doc, "/oml:evaluation_measures/oml:measures/oml:measure")
}
