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

  # FIXME: We currently support only parameter values that can be converted to character
  par.mode = vcapply(run$parameter.setting, function(x) mode(x$value))
  is.supported = par.mode %in% c("character", "logical", "numeric")
  if (any(!is.supported))
    stopf("parameters '%s' have mode '%s' which is currently not supported",
      collapse(names(par.mode[!is.supported]), ", "), collapse(par.mode[!is.supported], ", "))

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

  if (!is.null(run$mlr.benchmark.result)) {
    aggr = run$mlr.benchmark.result$results[[1]][[1]]$aggr

    output = newXMLNode("output_data", parent = top, namespace = "oml")

    # FIXME: maybe add time info for each resample iteration from `measures.test` slot
    eval.testtime = newXMLNode("evaluation", parent = output, namespace = "oml")
    mynode("name", "usercpu_time_millis_testing", parent = eval.testtime)
    mynode("flow", "openml.evaluation.usercpu_time_millis_testing(1.0)", parent = eval.testtime)
    mynode("value", aggr["timepredict.test.sum"], parent = eval.testtime)
    eval.traintime = newXMLNode("evaluation", parent = output, namespace = "oml")
    mynode("name", "usercpu_time_millis_training", parent = eval.traintime)
    mynode("flow", "openml.evaluation.usercpu_time_millis_training(1.0)", parent = eval.traintime)
    mynode("value", aggr["timetrain.test.sum"], parent = eval.traintime)
    eval.total = newXMLNode("evaluation", parent = output, namespace = "oml")
    mynode("name", "usercpu_time_millis", parent = eval.total)
    mynode("flow", "openml.evaluation.usercpu_time_millis(1.0)", parent = eval.total)
    mynode("value", sum(aggr[c("timetrain.test.sum", "timepredict.test.sum")]), parent = eval.total)
    # add scimark information
    if (!is.null(run$scimark.vector)) {
      eval.scimark = newXMLNode("evaluation", parent = output, namespace = "oml")
      mynode("name", "scimark_benchmark", parent = eval.scimark)
      mynode("flow", "openml.userdefined.scimark_benchmark(1.0)", parent = eval.scimark)
      mynode("value", run$scimark.vector[1L], parent = eval.scimark) # composite value
      mynode("array_data", paste0("[ ", collapse(run$scimark.vector[-1], sep = ", "), " ]"), parent = eval.scimark)
    }
    if ("cindex.test.mean" %in% names(aggr)) {
      eval = newXMLNode("evaluation", parent = output, namespace = "oml")
      mynode("name", "c_index", parent = eval)
      mynode("flow", "openml.evaluation.c_index(1.0)", parent = eval)
      mynode("value", aggr["cindex.test.mean"], parent = eval)
      ind = which(colnames(run$mlr.benchmark.result$results[[1]][[1]]$measures.test) == "cindex")
      mynode("stdev", sd(run$mlr.benchmark.result$results[[1]][[1]]$measures.test[, ind]), parent = eval)
    }
  }

  saveXML(top, file = file)
}
