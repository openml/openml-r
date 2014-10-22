#' Download OpenML task results from the server.
#'
#' @param id [\code{numeric}]\cr
#'   The task ID.
#' @param dir [\code{character(1)}]\cr
#'   The directory where to save the downloaded run xml. The file is called "results_task_id.xml" where "id" is
#'   replaced by the actual id. Default is the working directory.
#' @template arg_showinfo
#' @param clean.up [\code{logical(1)}]\cr
#'   Should the downloaded xml file be removed at the end?
#'   Default is \code{TRUE}.
#' @return [\code{\link{OpenMLTaskResults}}]
#' @export
downloadOpenMLTaskResults = function(id, ignore.cache = FALSE, show.info = getOpenMLOption("show.info")) {
  id = asCount(id)
  fn = file.path("results", "id", sprintf("%i.xml"))
  url = getAPIURL("openml.task.evaluations", task_id = id)
  contents = downloadXML(url, file = fn, ignore.cache = ignore.cache, show.info = show.info)
  doc = parseXMLResponse(contents, "Getting task results", "task_evaluations", as.text = TRUE)
  parseOpenMLTaskResults(doc)
}

parseOpenMLTaskResults = function(doc) {
  getMetrics = function(ns.runs) {
    task.res = list()
    for (i in seq_along(ns.runs)) {
      run.id = xmlRValI(doc, paste("/oml:task_evaluations/oml:evaluation",
        "[", i, "]/oml:run_id", sep=''))
      setup.id = xmlRValI(doc, paste("/oml:task_evaluations/oml:evaluation",
        "[", i, "]/oml:setup_id", sep=''))
      impl.id = xmlRValS(doc, paste("/oml:task_evaluations/oml:evaluation",
        "[", i, "]/oml:implementation_id", sep=''))
      impl = xmlRValS(doc, paste("/oml:task_evaluations/oml:evaluation",
        "[", i, "]/oml:implementation", sep=''))
      ns.metrics = getNodeSet(doc, paste("/oml:task_evaluations/oml:evaluation",
        "[", i, "]/oml:measure", sep=''))

      metric.names = unlist(lapply(ns.metrics, function(x) xmlGetAttr(x, "name")))
      metric.values = sapply(ns.metrics, function(x) xmlValue(x))

      task.res[[i]] = data.frame(run.id, setup.id, impl.id, impl, t(metric.values))
      colnames(task.res[[i]])[-(1:4)] = metric.names
    }
    # rbind task results and fill missing measures with <NA>
    metrics = do.call(rbind.fill, task.res)
    # cols of metrics are factors now, convert them:
    metrics = as.data.frame(lapply(metrics, function(x) type.convert(as.character(x))))
    return(metrics)
  }
  task.id = xmlRValI(doc, "/oml:task_evaluations/oml:task_id")
  task.name = xmlRValS(doc, "/oml:task_evaluations/oml:task_name")
  task.type.id = xmlRValI(doc, "/oml:task_evaluations/oml:task_type_id")
  input.data = xmlRValI(doc, "/oml:task_evaluations/oml:input_data")
  estim.proc = xmlRValS(doc, "/oml:task_evaluations/oml:estimation_procedure")

  ns.runs = getNodeSet(doc, "/oml:task_evaluations/oml:evaluation")
  if (length(ns.runs) != 0L) {
    metrics = getMetrics(ns.runs)
  } else {
    metrics = data.frame()
  }

  results = makeOpenMLTaskResults(
    task.id = task.id,
    task.name = task.name,
    task.type.id = task.type.id,
    input.data = input.data,
    estimation.procedure = estim.proc,
    metrics = metrics
  )
  return(results)
}
