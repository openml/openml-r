.listOMLRuns = function(task.id = NULL, flow.id = NULL,
  run.id = NULL, uploader.id = NULL, tag = NULL, verbosity = NULL) {
  if (!is.null(task.id)) asCount(task.id)
  if (!is.null(flow.id)) asCount(flow.id)
  if (!is.null(run.id)) asCount(run.id)
  if (!is.null(uploader.id)) asCount(uploader.id)
  if (!is.null(tag)) assertString(tag, na.ok = FALSE)
  if (is.null(task.id) && is.null(flow.id) && is.null(run.id) && is.null(uploader.id) && is.null(tag))
    stop("Please hand over at least one of the following: task.id, flow.id, run.id, uploader.id, tag")

  api.call = "run/list"
  if (!is.null(tag)) {
    api.call = collapse(c(api.call, "tag", tag), sep = "/")
  } else {
    if (length(run.id) > 1)
    run.id = collapse(run.id)
    if (length(task.id) > 1)
      task.id = collapse(task.id)
    if (length(flow.id) > 1)
      flow.id = collapse(flow.id)
    if (length(uploader.id) > 1)
      uploader.id = collapse(uploader.id)
    url.args = list(task = task.id, flow = flow.id, run = run.id, uploader = uploader.id)
    url.args = Filter(function(x) !is.null(x), url.args)

    api.call = paste0(api.call, "/", collapseNamedList(url.args, sep = "/", collapse = "/"))
  }

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
#' This function returns information on all OpenML runs that match certain
#' \code{task.id}(s), \code{run.id}(s), flow ID \code{flow.id} and/or
#' \code{uploader.id}(s). Alternatively the function can be passed a single
#' \code{tag} to list only runs with the corresponding tag associated.
#'
#' @template note_memoise
#'
#' @template arg_task_id
#' @template arg_flow.id
#' @param run.id [\code{integer}]\cr
#'  a single ID or a vector of IDs of the runs.
#' @param uploader.id [\code{integer(1)}]\cr
#'   ID of the uploader.
#' @template arg_tag
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @family run related functions
#' @export
#' @example inst/examples/listOMLRuns.R
listOMLRuns = memoise(.listOMLRuns)
