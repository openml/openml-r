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
#' @family listing functions
#' @family run related functions
#' @export
listOMLRuns = function(task.id = NULL, setup.id = NULL, flow.id = NULL, verbosity = NULL) {
  if (!is.null(task.id)) assertInt(task.id)
  if (!is.null(setup.id)) assertInt(setup.id)
  if (!is.null(flow.id)) assertInt(flow.id)
  if (is.null(task.id) && is.null(setup.id) && is.null(flow.id))
    stop("Please hand over at least one of the following: task.id, setup.id, flow.id")

  url.args = list(task_id = task.id, setup_id = setup.id, flow_id = flow.id)
  url.args = Filter(function(x) !is.null(x), url.args)

  content = try(doAPICall("run/list", url.args = url.args, file = NULL, method = "GET",
   verbosity = verbosity))

  if (is.error(content))
    return(data.frame())

  xml = parseXMLResponse(content, "Getting runs", "runs", as.text = TRUE)
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  as.data.frame(rename(rbindlist(lapply(blocks, function(node) {
    lapply(xmlChildren(node), function(x) as.integer(xmlValue(x)))
  }), fill = TRUE)))
}
