#' @title List OpenML runs.
#'
#' @description
#' This function returns information on all OpenML runs that match a certain
#' \code{task.id}, \code{setup.id} and/or implementation ID \code{impl.id}.
#'
#' @param task.id [\code{integer(1)}]\cr
#'  ID of the task.
#' @param setup.id [\code{integer(1)}]\cr
#'  ID of the parameter setup.
#' @param impl.id [\code{integer(1)}]\cr
#'  ID of the implementation.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
listOMLRuns = function(task.id = NULL, setup.id = NULL, impl.id = NULL, session.hash = getSessionHash(),
  verbosity = NULL) {

  if (!is.null(task.id)) assertInt(task.id)
  if (!is.null(setup.id)) assertInt(setup.id)
  if (!is.null(impl.id)) assertInt(impl.id)
  if (is.null(task.id) && is.null(setup.id) && is.null(impl.id))
    stop("Please hand over at least one of the following: task.id, setup.id, impl.id")
  assertString(session.hash)

  url = getAPIURL("openml.runs")
  content = try(downloadXML(url, NULL, verbosity = verbosity,
    session_hash = session.hash,
    task_id = task.id,
    setup_id = setup.id,
    implementation_id = impl.id), silent = TRUE)

  if (is.error(content))
    return(data.frame())

  xml = parseXMLResponse(content, "Getting runs", "runs", as.text = TRUE)
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  as.data.frame(rename(rbindlist(lapply(blocks, function(node) {
    lapply(xmlChildren(node), function(x) as.integer(xmlValue(x)))
  }), fill = TRUE)))
}
