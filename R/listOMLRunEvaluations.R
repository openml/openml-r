.listOMLRunEvaluations = function(task.id = NULL, flow.id = NULL,
  run.id = NULL, uploader.id = NULL, tag = NULL, verbosity = NULL) {
  
  api.call = generateAPICall(api.call = "evaluation/list", task.id = task.id, flow.id = flow.id,
    run.id = run.id, uploader.id = uploader.id, tag = tag)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)

  d = parseXMLResponse(content, "Getting task results", "evaluations", as.text = TRUE, return.doc = FALSE)

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
