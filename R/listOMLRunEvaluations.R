.listOMLRunEvaluations = function(task.id = NULL, flow.id = NULL, run.id = NULL,
  uploader.id = NULL, tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {

  if (is.null(task.id) && is.null(flow.id) && is.null(run.id) && is.null(uploader.id) && is.null(tag))
    stop("Please hand over at least one of the following: task.id, flow.id, run.id, uploader.id, tag")

  api.call = generateAPICall(api.call = "json/evaluation/list", task.id = task.id, flow.id = flow.id,
    run.id = run.id, uploader.id = uploader.id, tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)

  evals = fromJSON(txt = content)$evaluations$evaluation

  # convert long format to wide format
  evals = reshape(evals,
    timevar = "function",
    idvar = c("run_id", "task_id", "setup_id", "flow_id"),
    direction = "wide")

  # drop "all NA" columns
  evals = evals[, vlapply(evals, function(x) !all(is.na(x)))]

  # unfortunately column names are f***ed up now. Some tedious work is neccessary
  # to achive our naming conventions
  colnames(evals) = gsub("value[.]", "", colnames(evals))
  arr.ind = grepl("array_data[.]", colnames(evals))
  colnames(evals)[arr.ind] = paste0(gsub("array_data[.]", "", colnames(evals)[arr.ind]), ".array")

  # convert types (by default all is character)
  evals = as.data.frame(lapply(evals, type.convert, numerals = "no.loss", as.is = TRUE))

  # finally convert _ to . in col names
  names(evals) = convertNamesOMLToR(names(evals))

  return(evals)
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
