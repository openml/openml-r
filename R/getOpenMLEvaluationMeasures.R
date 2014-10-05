#' Get all names of OpenML evaluation measures.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console? Default is \code{FALSE}.
getOpenMLEvaluationMeasures = function(show.info = FALSE) {
  file = tempfile()
  on.exit({
    unlink(file)
  })
  downloadAPICallFile(api.fun = "openml.evaluation.measures", file = file, show.info = show.info)
  doc = parseXMLResponse(file, "Getting names of evaluation measures", "evaluation_measures")
  ems = xmlValsMultNsS(doc, "/oml:evaluation_measures/oml:measures/oml:measure")
  return(ems)
}