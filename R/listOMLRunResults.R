#' @title List run results of a task.
#'
#' @description
#' Retrieves all run results for a task specified by \code{task.id} and returns
#' it in a \code{data.frame}. Each row contains, among others, the run id \dQuote{rid},
#' the setup id \dQuote{sid} and the task.id \dQuote{tid}.
#'
#' @template arg_task_id
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family list
#' @export
listOMLRunResults = function(task.id, session.hash = getSessionHash(), verbosity = NULL) {
  id = asCount(task.id)
  assertString(session.hash)

  url = getAPIURL("openml.task.evaluations", task_id = id)
  content = try(downloadXML(url, NULL, verbosity, session_hash = session.hash), silent = TRUE)

  if (is.error(content))
    return(data.frame())

  doc = parseXMLResponse(content, "Getting task results", "task_evaluations", as.text = TRUE)

  # parse general information
  task.id = xmlRValI(doc, "/oml:task_evaluations/oml:task_id")
  task.name = xmlRValS(doc, "/oml:task_evaluations/oml:task_name")
  task.type.id = xmlRValI(doc, "/oml:task_evaluations/oml:task_type_id")
  input.data = xmlRValI(doc, "/oml:task_evaluations/oml:input_data")
  estim.proc = xmlRValS(doc, "/oml:task_evaluations/oml:estimation_procedure")

  # parse metrics
  ns.runs = getNodeSet(doc, "/oml:task_evaluations/oml:evaluation")
  if (length(ns.runs) == 0L)
    return(data.frame())

  run.info = vector("list", length(ns.runs))
  metrics = vector("list", length(ns.runs))
  for (i in seq_along(ns.runs)) {
    path.evals = "/oml:task_evaluations/oml:evaluation["
    run.info[[i]] = list(
      run.id = xmlRValI(doc, paste0(path.evals, i, "]/oml:run_id")),
      setup.id = xmlRValI(doc, paste0(path.evals, i, "]/oml:setup_id")),
      implementation.id = xmlRValI(doc, paste0(path.evals, i, "]/oml:implementation_id")),
      implementation = xmlRValS(doc, paste0(path.evals, i, "]/oml:implementation"))
    )
    metrics[[i]] = convertNodeSetToList(getNodeSet(doc, paste0(path.evals, i, "]/oml:measure")))
  }
  run.info = rbindlist(run.info)
  task.info = data.table(task.id = task.id, task.type.id = task.type.id, estim.proc = estim.proc)
  metrics = rbindlist(metrics, use.names = TRUE, fill = TRUE)
  for (cn in colnames(metrics))
    metrics[, cn := type.convert(metrics[[cn]], as.is = TRUE), with = FALSE]
  as.data.frame(rename(cbind(run.info, task.info, metrics)))
}
