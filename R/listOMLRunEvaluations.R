.listOMLRunEvaluations = function(task.id = NULL, flow.id = NULL,
  run.id = NULL, uploader.id = NULL, tag = NULL, verbosity = NULL) {
  if (!is.null(task.id)) asCount(task.id)
  if (!is.null(flow.id)) asCount(flow.id)
  if (!is.null(run.id)) asCount(run.id)
  if (!is.null(uploader.id)) asCount(uploader.id)
  if (!is.null(tag)) assertString(tag, na.ok = FALSE)
  if (is.null(task.id) && is.null(flow.id) && is.null(run.id) && is.null(uploader.id) && is.null(tag))
    stop("Please hand over at least one of the following: task.id, flow.id, run.id, uploader.id, tag")

  api.call = "evaluation/list"
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

  d = try(parseXMLResponse(content, "Getting task results", "evaluations", as.text = TRUE, return.doc = FALSE), silent = TRUE)
  if (is.error(d)) return(NULL)

  mat = xmlSApply(d, function(x) {
    line = getChildrenStringsNA(x)
    if ("array_data" %nin% names(line))
      line = c(line, "array_data" = NA)
    if ("value" %nin% names(line))
      line = c(line, "value" = NA)
    return(line)
  })

  mat = t(mat)
  ret = setNames(as.data.frame(unname(mat), stringsAsFactors = FALSE), colnames(mat))
  ret = reshape(ret, timevar = "function", idvar = c("run_id", "task_id", "setup_id", "flow_id"), direction = "wide")
  # remove NA columns
  ret = ret[,vlapply(ret, function(x) !all(is.na(x)))]

  colnames(ret) = gsub("value[.]", "", colnames(ret))
  arr.ind = grepl("array_data[.]", colnames(ret))
  colnames(ret)[arr.ind] = paste0(gsub("array_data[.]", "", colnames(ret)[arr.ind]), ".array")
  ret = as.data.frame(lapply(ret, type.convert, numerals = "no.loss", as.is = TRUE))

  colnames(ret) = gsub("_", ".", colnames(ret))
  return(ret)
}

#' @title List run results of a task.
#'
#' @description
#' Retrieves all run results for task(s) (\code{task.id}), flow(s) (\code{flow.id})
#' run(s) (\code{run.id}) or uploaders(s) (\code{uploader.id} and returns a \code{data.frame}.
#' Each row contains, among others, the run id \dQuote{rid}. Alternatively the
#' function can be passed a single \code{tag} to list only runs with the corresponding
#' tag associated.
#'
#' @template note_memoise
#'
#' @inheritParams listOMLRuns
#' @return [\code{data.frame}].
#' @family list
#' @export
#' @example inst/examples/listOMLRunEvaluations.R
listOMLRunEvaluations = memoise(.listOMLRunEvaluations)
