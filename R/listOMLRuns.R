#' @title List OpenML runs.
#'
#' @description
#' This function returns information on all OpenML runs that match a certain
#' \code{task.id}, \code{setup.id} and/or implementation ID \code{implementation.id}.
#'
#' @template arg_task_id
#' @param setup.id [\code{integer(1)}]\cr
#'  ID of the parameter setup.
#' @template arg_implementation.id
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @export
listOMLRuns = function(task.id = NULL, setup.id = NULL, implementation.id = NULL, verbosity = NULL) {
  if (!is.null(task.id)) assertInt(task.id)
  if (!is.null(setup.id)) assertInt(setup.id)
  if (!is.null(implementation.id)) assertInt(implementation.id)
  if (is.null(task.id) && is.null(setup.id) && is.null(implementation.id))
    stop("Please hand over at least one of the following: task.id, setup.id, implementation.id")

  #FIXME: JB: this stuff should be done in getAPIURL. Waiting for answers to my issue
  get.args = list(task.id = task.id, setup.id = setup.id, implementation.id = implementation.id)
  get.args = Filter(function(x) !is.null(x), get.args)
  get.args = collapse(paste(names(get.args), get.args, sep = "="), sep = "&")
  url = getAPIURL("run/list")
  url = paste(url, get.args, sep = "&")

  content = try(downloadXML(url, NULL, verbosity = verbosity), silent = TRUE)

  if (is.error(content))
    return(data.frame())

  xml = parseXMLResponse(content, "Getting runs", "runs", as.text = TRUE)
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  as.data.frame(rename(rbindlist(lapply(blocks, function(node) {
    lapply(xmlChildren(node), function(x) as.integer(xmlValue(x)))
  }), fill = TRUE)))
}
