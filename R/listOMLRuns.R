.listOMLRuns = function(task.id = NULL, flow.id = NULL, run.id = NULL,
  uploader.id = NULL, tag = NULL, limit = 5000, offset = NULL, verbosity = NULL) {

  if (is.null(task.id) && is.null(flow.id) && is.null(run.id) && is.null(uploader.id) && is.null(tag))
    stop("Please hand over at least one of the following: task.id, flow.id, run.id, uploader.id, tag")

  api.call = generateAPICall(api.call = "json/run/list", task.id = task.id, flow.id = flow.id,
    run.id = run.id, uploader.id = uploader.id, tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)
  if (is.null(content)) return(data.frame())
  # extract data frame
  runs = fromJSON(txt = content, simplifyVector = FALSE)$runs$run
  #tags = convertTagListToTagString(runs)
  runs = setDF(rbindlist(lapply(runs, function(x) x[c("run_id", "task_id", "setup_id", "flow_id", "uploader", "error_message")])))
  #runs$tags = tags
  names(runs) = convertNamesOMLToR(names(runs))

  # handle error messages
  runs$error.message = vcapply(runs$error.message, function(e) if (length(e) == 0L || !nzchar(e)) NA_character_ else e)

  # first five columns are IDs and hence need to be converted to integer
  #runs = as.data.frame(lapply(runs, type.convert, numerals = "no.loss", as.is = TRUE))
  #runs$error.message = as.factor(runs$error.message)

  # convert to integer
  i = stri_detect_fixed(colnames(runs), ".id")
  runs[i] = lapply(runs[i], as.integer)

  return(runs)
}

#' @title List the first 5000 OpenML runs.
#'
#' @description
#' This function returns information on all OpenML runs that match certain
#' \code{task.id}(s), \code{run.id}(s), flow ID \code{flow.id} and/or
#' \code{uploader.id}(s). Alternatively the function can be passed a single
#' \code{tag} to list only runs with the corresponding tag associated.
#' Note that by default only the first 5000 runs will be returned (due to the argument \dQuote{limit = 5000}).
#'
#' @template note_memoise
#'
#' @param task.id [\code{integer}]\cr
#'  a single ID or a vector of IDs of the task(s).
#' @param flow.id [\code{integer}]\cr
#'  a single ID or a vector of IDs of the flow(s).
#' @param run.id [\code{integer}]\cr
#'  a single ID or a vector of IDs of the run(s).
#' @param uploader.id [\code{integer}]\cr
#'  a single ID or a vector of IDs of uploader profile(s).
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
