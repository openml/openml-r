.listOMLRuns = function(task.id = NULL, setup.id = NULL, flow.id = NULL,
  run.id = NULL, uploader.id = NULL, verbosity = NULL) {
  if (!is.null(task.id)) assertInt(task.id)
  if (!is.null(setup.id)) assertInt(setup.id)
  if (!is.null(flow.id)) assertInt(flow.id)
  if (!is.null(run.id)) assertNumeric(run.id)
  if (!is.null(uploader.id)) assertInt(uploader.id)
  if (is.null(task.id) && is.null(setup.id) && is.null(flow.id) && is.null(run.id) && is.null(uploader.id))
    stop("Please hand over at least one of the following: task.id, setup.id, flow.id, run.id, uploader.id")

  if (length(run.id) > 1) run.id = collapse(run.id)
  url.args = list(task = task.id, setup = setup.id, flow = flow.id, run = run.id, uploader = uploader.id)
  url.args = Filter(function(x) !is.null(x), url.args)

  api.call = paste0("run/list/", collapseNamedList(url.args, sep = "/", collapse = "/"))
  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)

  # FIXME: speedup using return.doc = FALSE (see also listOMLRunResults)
  xml = try(parseXMLResponse(content, "Getting runs", "runs", as.text = TRUE), silent = TRUE)

  if (is.error(xml)) {
    return(NULL)
  }

  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  ret = as.data.frame(rename(rbindlist(lapply(blocks, function(node) {
    lapply(xmlChildren(node), function(x) (xmlValueNA(x)))
  }), fill = TRUE)))
  #int.vars = colnames(ret)[1:5]
  #ret[, int.vars] = lapply(int.vars, function(x) as.integer(ret[, x]))
  ret$error.message = as.character(ifelse(ret$error.message == "", NA, ret$error.message))

  as.data.frame(lapply(ret, type.convert, numerals = "no.loss", as.is = TRUE))
}

#' @title List OpenML runs.
#'
#' @description
#' This function returns information on all OpenML runs that match a certain
#' \code{task.id}, \code{setup.id} and/or implementation ID \code{flow.id}.
#'
#' @template note_memoise
#'
#' @template arg_task_id
#' @param setup.id [\code{integer(1)}]\cr
#'  ID of the parameter setup.
#' @template arg_flow.id
#' @param run.id [\code{integer}]\cr
#'  a single ID or a vector of IDs of the runs.
#' @param uploader.id [\code{integer(1)}]\cr
#'   ID of the uploader.
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @family run related functions
#' @export
#' @example inst/examples/listOMLRuns.R
listOMLRuns = memoise(.listOMLRuns)
