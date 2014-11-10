#' List all run results of a certain task.
#'
#' @param task.id [\code{numeric}]\cr
#'   The task ID.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}]
#' @export
listOMLRunResults = function(task.id, session.hash = getSessionHash(), verbosity = NULL) {
  id = asCount(task.id)

  url = getAPIURL("openml.task.evaluations", task_id = id)
  content = downloadXML(url, NULL, verbosity, session_hash = session.hash)
  doc = parseXMLResponse(content, "Getting task results", "task_evaluations", as.text = TRUE)

  # parse general information
  task.id = xmlRValI(doc, "/oml:task_evaluations/oml:task_id")
  task.name = xmlRValS(doc, "/oml:task_evaluations/oml:task_name")
  task.type.id = xmlRValI(doc, "/oml:task_evaluations/oml:task_type_id")
  input.data = xmlRValI(doc, "/oml:task_evaluations/oml:input_data")
  estim.proc = xmlRValS(doc, "/oml:task_evaluations/oml:estimation_procedure")

  # parse metrics
  ns.runs = getNodeSet(doc, "/oml:task_evaluations/oml:evaluation")
  if (length(ns.runs) != 0L) {
    run.info = c()
    run.res = vector("list", length(ns.runs))
    for (i in seq_along(ns.runs)) {
      path.evals = "/oml:task_evaluations/oml:evaluation["
      run.id = xmlRValI(doc, paste0(path.evals, i, "]/oml:run_id"))
      setup.id = xmlRValI(doc, paste0(path.evals, i, "]/oml:setup_id"))
      impl.id = xmlRValS(doc, paste0(path.evals, i, "]/oml:implementation_id"))
      impl = xmlRValS(doc, paste0(path.evals, i, "]/oml:implementation"))
      ns.metrics = getNodeSet(doc, paste0(path.evals, i, "]/oml:measure"))

      metric.names = unlist(lapply(ns.metrics, function(x) xmlGetAttr(x, "name")))
      metric.values = sapply(ns.metrics, function(x) xmlValue(x))

      run.info = rbind(run.info, data.frame(run.id, setup.id, impl.id, impl))
      run.res[[i]] = as.data.frame(t(metric.values))
      colnames(run.res[[i]]) = metric.names
    }
    # rbind run results and fill missing measures with <NA>
    metrics = do.call(rbind.fill, run.res)
    # cols of metrics are factors now, convert them:
    metrics = as.data.frame(lapply(metrics, function(x) type.convert(as.character(x))))

    task.info = lapply(data.frame(task.id, task.type.id, estim.proc), rep, times = nrow(metrics))
    metrics = cbind(run.info, task.info, metrics)
  } else {
    metrics = data.frame()
  }
  return(metrics)
}