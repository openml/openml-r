.listOMLTasks = function(task.type = NULL,
  estimation.procedure = NULL, evaluation.measures = NULL,
  number.of.instances = NULL, number.of.features = NULL,
  number.of.classes = NULL, number.of.missing.values = NULL,
  tag = NULL, data.name = NULL, data.tag = NULL,
  limit = 5000, offset = NULL, status = "active", verbosity = NULL) {

  estim.proc = listOMLEstimationProcedures(verbosity = 0)
  eval = listOMLEvaluationMeasures(verbosity = 0)

  if (!is.null(evaluation.measures))
    assertSubset(evaluation.measures, choices = as.character(eval$name))
  if (!is.null(estimation.procedure)) {
    assertSubset(estimation.procedure, choices = as.character(estim.proc$name))
    estimation.procedure = estim.proc$est.id[estim.proc$name %in% estimation.procedure]
  }

  api.call = generateAPICall("json/task/list",
    task.type = task.type, number.of.instances = number.of.instances,
    number.of.features = number.of.features, number.of.classes = number.of.classes,
    number.of.missing.values = number.of.missing.values,
    tag = tag, data.name = data.name, data.tag = data.tag,
    limit = limit, offset = offset, status = status)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")

  if (is.null(content)) return(data.frame())

  res = fromJSON(txt = content, simplifyVector = FALSE)$tasks$task
  input = convertNameValueListToDF(extractSubList(res, "input", simplify = FALSE))
  # get rid of less interesting stuff
  input = input[, which(colnames(input) %in% c("source_data", "target_value", "time_limit", "number_samples")) := NULL]
  qualities = convertNameValueListToDF(extractSubList(res, "quality", simplify = FALSE))
  # tags = convertTagListToTagString(res)
  # subset according to evaluation measure and estimation procedure
  ind.eval = ind.estim = rep(TRUE, nrow(input))
  if (!is.null(evaluation.measures))
    ind.eval = input$evaluation_measures %in% evaluation.measures
  if (!is.null(estimation.procedure))
    ind.estim = input$estimation_procedure %in% estimation.procedure

  # add NA columns for estimation and evaluation if missing
  if (is.null(input$estimation_procedure)) {
    input$estimation_procedure = NA
  } else {
    row.names(estim.proc) = estim.proc$est.id
    input$estimation_procedure = as.character(estim.proc[input$estimation_procedure , "name"])
  }
  if (is.null(input$evaluation_measures)) input$evaluation_measures = NA_character_

  # again get rid of redundant/uninteresting stuff
  res = rbindlist(lapply(res, function(x) x[c("task_id", "task_type", "did", "name", "status", "format")]))
  #vapply(res, FUN = function(x) unlist(x[c("task_id", "task_type", "did", "name", "status", "format")]), FUN.VALUE = character(6))
  #res$quality = res$input = res$tags = NULL

  # build final dataframe
  res = setDF(cbind(res, input, qualities))

  # convert to integer
  i = colnames(res) %in% c(colnames(qualities), "did", "task_id")
  res[i] = lapply(res[i], as.integer)

  # finally convert _ to . in col names
  names(res) = convertNamesOMLToR(names(res))

  return(res[ind.estim & ind.eval, ])
}

#' @title List the first 5000 OpenML tasks.
#'
#' @description
#' The returned \code{data.frame} contains the \code{task_id}, the data set id \code{data.id},
#' the \code{status} and some describing data qualities.
#' Note that by default only the first 5000 data sets will be returned (due to the argument \dQuote{limit = 5000}).
#'
#' @template note_memoise
#'
#' @param task.type [\code{character(1)}]\cr
#'   If not \code{NULL}, only tasks belonging to the given task type are listed.
#'   Use \code{listOMLTaskTypes()$name} to see possible values for \code{task.type}.
#'   The default is \code{NULL}, which means that tasks with all available task types are listed.
#' @param estimation.procedure [\code{character}]\cr
#'   If not \code{NULL}, only tasks belonging the given estimation procedures are listed.
#'   Use \code{listOMLEstimationProcedures()$name} to see possible values for
#'   \code{estimation.procedure}. The default is \code{NULL}, which means that tasks with all
#'   available estimation procedures are listed.
#' @param evaluation.measures [\code{character}]\cr
#'   If not \code{NULL}, only tasks belonging the given evaluation measures are listed.
#'   Use \code{listOMLEvaluationMeasures()$name} to see possible values for
#'   \code{evaluation.measures}. The default is \code{NULL}, which means that tasks with all
#'   available evaluation measures are listed.
#' @template arg_number.of.instances
#' @template arg_number.of.features
#' @template arg_number.of.classes
#' @template arg_number.of.missing.values
#' @template arg_tag
#' @template arg_data.name
#' @param data.tag [\code{character(1)}]\cr
#'   Refers to the tag of the dataset the task is based on.
#'   If not \code{NULL} only tasks with the corresponding \code{data.tag} are listed.
#' @template arg_limit
#' @template arg_offset
#' @template arg_status
#' @template arg_verbosity
#'
#' @return [\code{data.frame}].
#' @family listing functions
#' @family task-related functions
#' @export
#' @example inst/examples/listOMLTasks.R
listOMLTasks = memoise(.listOMLTasks)
