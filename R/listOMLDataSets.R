.listOMLDataSets = function(number.of.instances = NULL, number.of.features = NULL,
  number.of.classes = NULL, number.of.missing.values = NULL,
  tag = NULL, data.name = NULL,
  limit = 5000, offset = NULL, status = "active", verbosity = NULL) {

  api.call = generateAPICall("json/data/list",
    number.of.instances = number.of.instances, number.of.features = number.of.features,
    number.of.classes = number.of.classes, number.of.missing.values = number.of.missing.values,
    tag = tag, data.name = data.name,
    limit = limit, offset = offset, status = status)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  if (is.null(content)) return(data.frame())
  res = fromJSON(txt = content, simplifyVector = FALSE)$data$dataset

  qualities = convertNameValueListToDF(extractSubList(res, "quality", simplify = FALSE))
  tags = convertTagListToTagString(res)
  res = rbindlist(lapply(res, function(x) x[c("did", "name", "version", "status", "format")]))

  if (nrow(qualities) == 0L)
    res = setDF(cbind(res, tags)) else
      res = setDF(cbind(res, tags, qualities))

  # convert to integer
  i = colnames(res) %in% colnames(qualities)
  if (any(i))
    res[i] = lapply(res[i], as.integer)

  # finally convert _ to . in col names
  names(res) = convertNamesOMLToR(names(res))

  if (!is.null(limit) & (nrow(res) == limit))
    messagef("The limit to %i result(s) was achieved, you can use the 'limit' arg to increase the limit.", limit)

  return(res)
}

#' @title List the first 5000 OpenML data sets.
#'
#' @description
#' The returned \code{data.frame} contains the data set id \dQuote{data.id},
#' the \dQuote{status} (\dQuote{active}, \dQuote{deactivated}, \dQuote{in_preparation})
#' and describing data qualities.
#' Note that by default only the first 5000 data sets will be returned (due to the argument \dQuote{limit = 5000}).
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
#' @return [\code{data.frame}].
#' @family listing functions
#' @family data set-related functions
#' @export
#' @example inst/examples/listOMLDataSets.R
listOMLDataSets = memoise(.listOMLDataSets)
