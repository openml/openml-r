.listOMLDataSets = function(NumberOfInstances = NULL, NumberOfFeatures = NULL,
  NumberOfClasses = NULL, NumberOfMissingValues = NULL,
  tag = NULL, limit = NULL, offset = NULL, status = "active", verbosity = NULL) {
  assertSubset(status, getValidOMLDataSetStatusLevels())

  api.call = generateAPICall("json/data/list",
    NumberOfInstances = NumberOfInstances, NumberOfFeatures = NumberOfFeatures,
    NumberOfClasses = NumberOfClasses, NumberOfMissingValues = NumberOfMissingValues,
    tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  res = fromJSON(txt = content, simplifyVector = FALSE)$data$dataset

  qualities = convertNameValueListToDF(extractSubList(res, "quality", simplify = FALSE))
  #data.id = as.integer(extractSubList(res, "did"))
  res = rbindlist(lapply(res, function(x) x[c("did", "name", "version", "status", "format")])) 
  #res = cbind(data.id, as.data.frame(res, stringsAsFactors = FALSE), qualities, stringsAsFactors = FALSE)
 
  res = cbind(res, qualities)
  res = as.data.frame(res, stringsAsFactors = FALSE)
  
  # convert to integer
  i = colnames(res) %in% colnames(qualities)
  res[i] = lapply(res[i], as.integer)
  
  # finally convert _ to . in col names
  names(res) = convertNamesOMLToR(names(res))
  
  return(res)
}

#' @title List available OpenML data sets.
#'
#' @description
#' The returned \code{data.frame} contains the data set id \dQuote{data.id},
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
