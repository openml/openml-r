.listOMLRunEvaluations = function(task.id = NULL, flow.id = NULL, run.id = NULL,
  uploader.id = NULL, tag = NULL, limit = NULL, offset = NULL, verbosity = NULL, 
  show.array.measures = FALSE, pretty.flow.names = TRUE) {

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
  colnames(evals) = stri_replace_all_fixed(colnames(evals), "value_", "")
  arr.ind = stri_detect_fixed(colnames(evals), "array_data_")
  colnames(evals)[arr.ind] = paste0(stri_replace_all_fixed(colnames(evals)[arr.ind], "array_data_", ""), "_array")
  if (!show.array.measures) {
    evals = evals[,!arr.ind]
  }
  
  # convert types (by default all is character)
  #evals = as.data.frame(lapply(evals, type.convert, numerals = "no.loss", as.is = TRUE))

  # finally convert _ to . in col names
  names(evals) = convertNamesOMLToR(names(evals))
  
  flow.source = stri_replace_first(evals$flow.name, regex = "[.].*", replacement = "")
  flow.source = ifelse(flow.source %in% c("classif", "regr"), "mlr", flow.source)
  evals = as.data.frame(append(evals, after = which(names(evals) == "flow.name"),
    values = list(flow.source = flow.source)))
  
  if (pretty.flow.names) {
    evals$flow.name = stri_replace_all(evals$flow.name, regex = ".*[.]|\\(.*\\)", replacement = "")
  }
  
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
#' @param show.array.measures
#'  Should measures that return an array (i.e. confusion matrix, predictive accuracy per cv-fold) instead of a single skalar value be shown?
#' @param pretty.flow.names
#'  Should the version number of the flow and the prefix that specifies the software be removed?
#'   
#' @return [\code{data.frame}].
#' @family list
#' @export
#' @example inst/examples/listOMLRunEvaluations.R
listOMLRunEvaluations = memoise(.listOMLRunEvaluations)
