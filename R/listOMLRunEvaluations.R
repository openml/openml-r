.listOMLRunEvaluations = function(task.id = NULL, flow.id = NULL, run.id = NULL,
  uploader.id = NULL, tag = NULL, limit = NULL, offset = NULL, verbosity = NULL,
  evaluation.measure = NULL, show.array.measures = FALSE, extend.flow.name = TRUE) {

  if (is.null(task.id) && is.null(flow.id) && is.null(run.id) && is.null(uploader.id) && is.null(tag))
    stop("Please hand over at least one of the following: task.id, flow.id, run.id, uploader.id, tag")
  if (is.null(evaluation.measure))
    showInfo(verbosity, "Suggestion: Use the 'evaluation.measure' argument to restrict the results to only one measure.")

  api.call = generateAPICall(api.call = "json/evaluation/list", task.id = task.id,
    flow.id = flow.id, run.id = run.id, uploader.id = uploader.id,
    tag = tag, evaluation.measure = evaluation.measure, limit = limit, offset = offset)

  content = doAPICall(api.call, file = NULL, method = "GET", verbosity = verbosity)
  if (is.null(content)) return(data.frame())
  evals = fromJSON(txt = content, simplifyVector = FALSE)$evaluations$evaluation

  evals = rbindlist(lapply(evals, function(x) {
    if (is.null(x$value)) x$value = NA
    if (is.null(x$array_data)) x$array_data = NA else x$array_data = collapse(x$array_data)
    cols = c("run_id", "task_id", "setup_id", "flow_id", "flow_name", "data_name",
      "upload_time", "function", "value", "array_data")
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
    evals = evals[, !arr.ind]
  }

  # convert types (by default all is character)
  #evals = as.data.frame(lapply(evals, type.convert, numerals = "no.loss", as.is = TRUE))

  # finally convert _ to . in col names
  names(evals) = convertNamesOMLToR(names(evals))

  # split flow.name into learner.name, version and flow.source
  if (extend.flow.name) {
    # extract flow.version
    flow.version = stri_match_last(evals$flow.name,
      regex = "[[:digit:]]+\\.*[[:digit:]]*")

    # extract flow.source
    src = c("weka", "mlr", "moa", "sklearn", "rm", "HubMiner", "classif", "regr", "surv", "openml")
    src = stri_paste("^", src)
    flow.source = stri_match_first(evals$flow.name, regex = stri_paste(src, collapse = "|"))
    ind = flow.source %in% c("classif", "regr", "surv")
    flow.source[ind] = "mlr"
    #stri_match_first(evals$flow.name, regex = "^[[:alnum:]]+[[:alnum:]]")
    #stri_replace_first(evals$flow.name, replacement = "",  regex = "[.].*$")

    # extract learner.name
    learner.name = stri_replace_last(evals$flow.name, replacement = "",
      regex = "\\([[:digit:]]+\\.*[[:digit:]]*\\)")
    learner.name[!ind] = stri_replace_first(learner.name[!ind], replacement = "",
      regex = "^[[:alnum:]]+\\.*[.]")

    evals = as.data.frame(append(evals, after = which(names(evals) == "flow.name"),
      values = list(flow.version = flow.version, flow.source = flow.source, learner.name = learner.name)),
      stringsAsFactors = FALSE)
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
#' @param evaluation.measure [\code{character(1)}]\cr
#'  Use this to speedup your request.
#'  It restricts the results to only one evaluation measure (see \code{\link{listOMLEvaluationMeasures}} for possible values).
#'  Default is \code{NULL}, which means that no restriction is going to happen and all possible evaluation measures will be returned.
#' @param show.array.measures [\code{logical(1)}]\cr
#'  Should measures that return an array instead of a single skalar value be shown (e.g. confusion matrix, predictive accuracy within each class)?
#'  Default is \code{FALSE}.
#' @param extend.flow.name [\code{logical(1)}]\cr
#'  Adds a column \code{flow.version} that refers to the version number of the flow and a column \code{flow.source} containing the prefix of the flow that specifies the source of the flow (i.e. weka, R) and a column \code{learner.name} that refers to the learner.
#'  Default is \code{TRUE}.
#'
#' @return [\code{data.frame}].
#' @family list
#' @export
#' @example inst/examples/listOMLRunEvaluations.R
listOMLRunEvaluations = memoise(.listOMLRunEvaluations)
