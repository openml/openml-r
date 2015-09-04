# Generate an XML file for an OMLRun object.
#
# @param run [\code{\link{OMLRun}}]\cr
#   The run.
# @param file [\code{character(1)}]\cr
#   Destination path where the XML file should be saved.
# @return [\code{invisible(NULL)}].
writeOMLRunXML = function(run, file) {
  assertClass(run, "OMLRun")
  assertPathForOutput(file, overwrite = TRUE)

  doc = newXMLDoc()
  top = newXMLNode("oml:run", parent = doc, namespace = c(oml = "http://openml.org/openml"))

  mynode = function(name, val, parent = top) {
    if (!is.na(val))
      newXMLNode(name, as.character(val), parent = parent, namespace = "oml")
  }

  mynode("task_id", run$task.id)
  mynode("flow_id", run$flow.id)
  mynode("error_message", run$error.message)

  for (i in seq_along(run$parameter.setting)) {
    par.setting = newXMLNode("parameter_setting", parent = top, namespace = "oml")
    mynode("name", run$parameter.setting[[i]]$name, parent = par.setting)
    mynode("value", run$parameter.setting[[i]]$value, parent = par.setting)
    mynode("component", run$parameter.setting[[i]]$component, parent = par.setting)
  }
  aggr = run$mlr.resample.result$aggr
  if ("cindex.test.mean" %in% names(aggr)) {
    output = newXMLNode("output_data", parent = top, namespace = "oml")
    eval = newXMLNode("evaluation", parent = output, namespace = "oml")
    mynode("name", "c_index", parent = eval)
    mynode("flow", "openml.evaluation.c_index(1.0)", parent = eval)
    mynode("value", aggr, parent = eval)
    ind = which(colnames(run$mlr.resample.result$measures.test) == "cindex")
    mynode("stdev", sd(run$mlr.resample.result$measures.test[, ind]), parent = eval)
  }
  saveXML(top, file = file)
}
