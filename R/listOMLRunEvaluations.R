.listOMLRunEvaluations = function(task.id = NULL, flow.id = NULL, run.id = NULL,
  uploader.id = NULL, tag = NULL, limit = NULL, offset = NULL, verbosity = NULL) {

  if (is.null(task.id) && is.null(flow.id) && is.null(run.id) && is.null(uploader.id) && is.null(tag))
    stop("Please hand over at least one of the following: task.id, flow.id, run.id, uploader.id, tag")

  api.call = generateAPICall(api.call = "json/evaluation/list", task.id = task.id, flow.id = flow.id,
    run.id = run.id, uploader.id = uploader.id, tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)

  evals = fromJSON(txt = content, simplifyVector = FALSE)$evaluations$evaluation

  evals = rbindlist(lapply(evals, function(x) {
    if(is.null(x$value)) x$value = NA
    if(is.null(x$array_data)) x$array_data = NA else x$array_data = collapse(x$array_data)
    cols = c("run_id", "task_id", "setup_id", "flow_id", "flow_name", "data_name", "upload_time", "function", "value", "array_data")
    x[cols]
  }))

  # convert long format to wide format
  setnames(evals, "function", "fun")
  form = run_id + task_id + setup_id + flow_id + flow_name + data_name + upload_time ~ fun
  evals = dcast(data = evals, formula = form, value.var = c("value", "array_data"))

  # drop "all NA" columns
  evals = as.data.frame(evals)[, vlapply(evals, function(x) !all(is.na(x)))]
  # drop all array columns that are NULL
  #drop.array = vlapply(evals[,grepl("array_data[_]", colnames(evals))], function(x) all(vlapply(x, is.null)))
  #drop.array = names(drop.array)[drop.array]
  #evals = evals[, colnames(evals)%nin%drop.array]
  
  # unfortunately column names are f***ed up now. Some tedious work is neccessary
  # to achive our naming conventions
  colnames(evals) = gsub("value[_]", "", colnames(evals))
  arr.ind = grepl("array_data[_]", colnames(evals))
  colnames(evals)[arr.ind] = paste0(gsub("array_data[_]", "", colnames(evals)[arr.ind]), "_array")

  # convert types (by default all is character)
  #evals = as.data.frame(lapply(evals, type.convert, numerals = "no.loss", as.is = TRUE))

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
