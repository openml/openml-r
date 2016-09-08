.listOMLTasks = function(number.of.instances = NULL, number.of.features = NULL,
  number.of.classes = NULL, number.of.missing.values = NULL,
  tag = NULL, data.name = NULL,
  limit = NULL, offset = NULL, status = "active", verbosity = NULL) {
  assertSubset(status, getValidOMLDataSetStatusLevels())

  api.call = generateAPICall("json/task/list",
    number.of.instances = number.of.instances, number.of.features = number.of.features,
    number.of.classes = number.of.classes, number.of.missing.values = number.of.missing.values,
    tag = tag, data.name = data.name,
    limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")

  res = fromJSON(txt = content, simplifyVector = FALSE)$tasks$task
  input = convertNameValueListToDF(extractSubList(res, "input", simplify = FALSE))
  qualities = convertNameValueListToDF(extractSubList(res, "quality", simplify = FALSE))
  tags = vcapply(extractSubList(res, "tags", simplify = FALSE), function(x) collapse(unlist(x), ", "))

  # get rid of less interesting stuff
  #input$source_data = input$target_value = input$time_limit = input$number_samples = NULL
  input = input[, which(colnames(input)%in%c("source_data", "target_value", "time_limit", "number_samples")):=NULL]

  # include columns for estimation and evaluation if missing
  if (is.null(input$estimation_procedure)) input$estimation_procedure = NA
  if (is.null(input$evaluation_measures)) input$evaluation_measures = NA_character_

  # again get rid of redundant/uninteresting stuff
  res = rbindlist(lapply(res, function(x) x[c("task_id", "task_type", "did", "name", "status", "format")]))
  #vapply(res, FUN = function(x) unlist(x[c("task_id", "task_type", "did", "name", "status", "format")]), FUN.VALUE = character(6))
  #res$quality = res$input = res$tags = NULL

  # build final dataframe
  res = cbind(res, input, qualities, tags)
  res = as.data.frame(res, stringsAsFactors = FALSE)

  # convert to integer
  i = colnames(res) %in% c(colnames(qualities), "did", "task_id")
  res[i] = lapply(res[i], as.integer)

  # finally convert _ to . in col names
  names(res) = convertNamesOMLToR(names(res))

  return(res)
}

#' @title List available OpenML tasks.
#'
#' @description
#' The returned \code{data.frame} contains the \code{task_id}, the data set id \code{data.id},
#' the \code{status} and some describing data qualities.
#'
#' @template note_memoise
#'
#' @template arg_number.of.instances
#' @template arg_number.of.features
#' @template arg_number.of.classes
#' @template arg_number.of.missing.values
#' @template arg_tag
#' @template arg_data.name
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
