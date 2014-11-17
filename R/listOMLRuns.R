#' @title List OpenML runs.
#'
#' @description This function returns information on all OpenML runs that match a certain
#' \code{task.id}, \code{setup.id} and/or implementation ID \code{impl.id}.
#'
#' @param task.id [\code{integer}]\cr
#'
#' @param setup.id [\code{integer(1)}]\cr
#'
#' @param impl.id [\code{integer(1)}]\cr
#'
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
listOMLRuns = function(task.id = NULL, setup.id = NULL, impl.id = NULL, session.hash = getSessionHash(),
  verbosity = NULL) {

  if (!is.null(task.id)) assertIntegerish(task.id)
  if (!is.null(setup.id)) assertIntegerish(setup.id)
  if (!is.null(impl.id)) assertIntegerish(impl.id)

  url = getAPIURL("openml.runs")
  content = downloadXML(url, NULL, verbosity = verbosity, 
    session_hash = session.hash, 
    task_id = task.id, 
    setup_id = setup.id,
    implementation_id = impl.id)
  xml = parseXMLResponse(content, "Getting runs", "runs", as.text = TRUE)

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  runs = rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    qualities = names(children) == "run"
    row = c(
      list(
        as.integer(xmlValue(children[["run_id"]])),
        as.integer(xmlValue(children[["task_id"]])),
        as.integer(xmlValue(children[["setup_id"]])),
        as.integer(xmlValue(children[["implementation_id"]])),
        as.integer(xmlValue(children[["uploader"]]))
      ),
      as.list(as.integer(vcapply(children[qualities], xmlValue)))
    )
    names(row) = c("run.id", "task.id", "setup.id", "impl.id", "uploader")
    row
  }), fill = TRUE)

  as.data.frame(runs)
}
