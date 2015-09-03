#' @title List OpenML runs.
#'
#' @description
#' This function returns information on all OpenML runs that match a certain
#' \code{task.id}, \code{setup.id} and/or implementation ID \code{flow.id}.
#'
#' @template arg_task_id
#' @param setup.id [\code{integer(1)}]\cr
#'  ID of the parameter setup.
#' @template arg_flow.id
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
listOMLRuns = function(task.id = NULL, setup.id = NULL, flow.id = NULL, verbosity = NULL) {
  if (!is.null(task.id)) assertInt(task.id)
  if (!is.null(setup.id)) assertInt(setup.id)
  if (!is.null(flow.id)) assertInt(flow.id)
  if (is.null(task.id) && is.null(setup.id) && is.null(flow.id))
    stop("Please hand over at least one of the following: task.id, setup.id, flow.id")

  #FIXME: API expects implementation.id here instead of flow.id
  get.args = list(task.id = task.id, setup.id = setup.id, implementation.id = flow.id)
  get.args = Filter(function(x) !is.null(x), get.args)
  url = getAPIURL("run/list", get.args = get.args)

  content = try(doAPICallGET(url, NULL, verbosity = verbosity), silent = TRUE)

  if (is.error(content))
    return(data.frame())

  xml = parseXMLResponse(content, "Getting runs", "runs", as.text = TRUE)
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  as.data.frame(rename(rbindlist(lapply(blocks, function(node) {
    lapply(xmlChildren(node), function(x) as.integer(xmlValue(x)))
  }), fill = TRUE)))
}
