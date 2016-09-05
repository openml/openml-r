.listOMLRuns = function(task.id = NULL, flow.id = NULL, run.id = NULL,
  uploader.id = NULL, tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {

  if (is.null(task.id) && is.null(flow.id) && is.null(run.id) && is.null(uploader.id) && is.null(tag))
    stop("Please hand over at least one of the following: task.id, flow.id, run.id, uploader.id, tag")

  api.call = generateAPICall(api.call = "json/run/list", task.id = task.id, flow.id = flow.id,
    run.id = run.id, uploader.id = uploader.id, tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)

  # extract data frame
  runs = fromJSON(txt = content)$runs$run

  names(runs) = convertNamesOMLToR(names(runs))

  # handle error messages
  runs$error.message = sapply(runs$error.message, function(e) if (identical(e, character(0)) || e == "") NA else e)

  # first five columns are IDs and hence need to be converted to integer
  runs = as.data.frame(lapply(runs, type.convert, numerals = "no.loss", as.is = TRUE))

  return(runs)
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
#' @template arg_limit
#' @template arg_offset
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @family listing functions
#' @family run-related functions
#' @export
#' @example inst/examples/listOMLRuns.R
listOMLRuns = memoise(.listOMLRuns)
