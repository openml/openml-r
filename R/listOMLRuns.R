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

  #url.args = list(task_id = task.id, setup_id = setup.id, flow_id = flow.id)
  url.args = list(task = task.id, setup = setup.id, flow = flow.id)
  url.args = Filter(function(x) !is.null(x), url.args)

  api.call = paste0("run/list/", collapseNamedList(url.args, sep = "/", collapse = "/"))
  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)

  xml = try(parseXMLResponse(content, "Getting runs", "runs", as.text = TRUE), silent = TRUE)
  
  if (is.error(xml))
    return(data.frame())
  
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  ret = as.data.frame(rename(rbindlist(lapply(blocks, function(node) {
    lapply(xmlChildren(node), function(x) (xmlValue(x)))
  }), fill = TRUE)))
  int.vars = colnames(ret)[1:5]
  ret[, int.vars] = lapply(int.vars, function(x) as.integer(ret[, x]))
  ret$error.message = ifelse(ret$error.message == "", NA, as.character(ret$error.message))
  return(ret)
}
