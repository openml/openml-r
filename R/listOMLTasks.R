.listOMLTasks = function(NumberOfInstances = NULL, NumberOfFeatures = NULL,
  NumberOfClasses = NULL, NumberOfMissingValues = NULL,
  tag = NULL, limit = NULL, offset = NULL, status = "active", verbosity = NULL) {
  assertSubset(status, getValidOMLDataSetStatusLevels())

  api.call = generateAPICall("json/task/list",
    NumberOfInstances = NumberOfInstances, NumberOfFeatures = NumberOfFeatures,
    NumberOfClasses = NumberOfClasses, NumberOfMissingValues = NumberOfMissingValues,
    tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")

  res = fromJSON(txt = content)$tasks$task
  res$task_type_id = NULL

  # make some neccesary conversions (we want a compact data frame)
  qualities = convertNameValueListToDF(res$quality)
  input = convertNameValueListToDF(res$input)

  # get rid of less interesting stuff
  input$source_data = input$target_value = input$time_limit = input$number_samples = NULL

  # include columns for estimation and evaluation if missing
  if (is.null(input$estimation_procedure)) input$estimation_procedure = NA
  if (is.null(input$evaluation_measures)) input$evaluation_measures = NA_character_

  # build tag string
  if (is.list(res$tag)) {
    tags = vcapply(res$tag, function(x) collapse(x, sep = ", "))
  } else {
    tags = collapse(res$tag, sep = ", ")
  }

  # again get rid of redundant/uninteresting stuff
  res$quality = res$input = res$tag = NULL

  # build final dataframe
  res = cbind(res, input, qualities, tags, stringsAsFactors = FALSE)

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
#' The returned \code{data.frame} contains the \code{task_id}, the data set id \code{did},
#' the \code{status} and some describing data qualities.
#'
#' @template note_memoise
#'
#' @template arg_NumberOfInstances
#' @template arg_NumberOfFeatures
#' @template arg_NumberOfClasses
#' @template arg_NumberOfMissingValues
#' @template arg_tag
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
