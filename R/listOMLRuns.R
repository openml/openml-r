.listOMLRuns = function(task.id = NULL, flow.id = NULL,
  run.id = NULL, uploader.id = NULL, tag = NULL, verbosity = NULL) {

  api.call = generateAPICall(api.call = "run/list", task.id = task.id, flow.id = flow.id,
    run.id = run.id, uploader.id = uploader.id, tag = tag)
  
  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)

  # FIXME: speedup using return.doc = FALSE (see also listOMLRunResults)
  xml = try(parseXMLResponse(content, "Getting runs", "runs", as.text = TRUE))

  if (is.error(xml)) {
    return(data.frame())
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
#' @family run-related functions
#' @export
#' @example inst/examples/listOMLRuns.R
listOMLRuns = memoise(.listOMLRuns)
