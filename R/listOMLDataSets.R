.listOMLDataSets = function(NumberOfInstances = NULL, NumberOfFeatures = NULL,
  NumberOfClasses = NULL, NumberOfMissingValues = NULL,
  tag = NULL, limit = NULL, offset = NULL, status = "active", verbosity = NULL) {
  assertSubset(status, getValidOMLDataSetStatusLevels())

  api.call = generateAPICall("json/data/list",
    NumberOfInstances = NumberOfInstances, NumberOfFeatures = NumberOfFeatures,
    NumberOfClasses = NumberOfClasses, NumberOfMissingValues = NumberOfMissingValues,
    tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  res = fromJSON(txt = content)[[1L]][[1L]]

  data.id = as.integer(res$did)
  qualities = convertNameValueListToDF(res$quality)
  res$quality = res$did = NULL

  res = cbind(data.id, as.data.frame(res, stringsAsFactors = FALSE), qualities, stringsAsFactors = FALSE)
  i = colnames(res) %in% colnames(qualities)
  res[i] = lapply(res[i], as.integer)

  return(res)
}

convertNameValueListToDF = function(x) {
  if (!isTRUE(checkList(x))) x = list(x)
  ret = lapply(x, function(x) as.list(setNames(x$value, x$name)))
  cols = unique(unlist(lapply(x, function(x) x$name)))
  # replace empty rows with NA
  na.ind = which(vnapply(ret, length) == 0)
  ret[na.ind] = lapply(seq_along(na.ind), function(x) as.list(setNames(rep(NA, length(cols)), cols)))
  ret = as.data.frame(rbindlist(ret, fill = TRUE), stringsAsFactors = FALSE)
  return(ret)
}

#' @title List available OpenML data sets.
#'
#' @description
#' The returned \code{data.frame} contains the data set id \dQuote{did},
#' the \dQuote{status} (\dQuote{active}, \dQuote{deactivated}, \dQuote{in_preparation})
#' and describing data qualities.
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
#' @return [\code{data.frame}].
#' @family listing functions
#' @family data set-related functions
#' @export
#' @example inst/examples/listOMLDataSets.R
listOMLDataSets = memoise(.listOMLDataSets)
