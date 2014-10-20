#' Get all names of OpenML evaluation measures.
#' @template arg_showinfo
getOpenMLEvaluationMeasures = function(show.info = getOpenMLOption("show.info")) {
  file = tempfile()
  on.exit({
    unlink(file)
  })
  downloadAPICallFile(api.fun = "openml.evaluation.measures", file = file, show.info = show.info)
  doc = parseXMLResponse(file, "Getting names of evaluation measures", "evaluation_measures")
  ems = xmlValsMultNsS(doc, "/oml:evaluation_measures/oml:measures/oml:measure")
  return(ems)
}
