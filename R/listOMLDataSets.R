.listOMLDataSets = function(number.of.instances = NULL, number.of.features = NULL,
  number.of.classes = NULL, number.of.missing.values = NULL,
  tag = NULL, limit = NULL, offset = NULL, status = "active", verbosity = NULL) {
  assertSubset(status, getValidOMLDataSetStatusLevels())

  api.call = generateAPICall("data/list",
    NumberOfInstances = number.of.instances, NumberOfFeatures = number.of.features,
    NumberOfClasses = number.of.classes, NumberOfMissingValues = number.of.missing.values,
    tag = tag, limit = limit, offset = offset)

  content = doAPICall(api.call = api.call, file = NULL, verbosity = verbosity, method = "GET")
  xml = parseXMLResponse(content, "Getting data set list", "data", as.text = TRUE)

  # get list of blocks for data sets
  blocks = xmlChildren(xmlChildren(xml)[[1L]])
  df = as.data.frame(rbindlist(lapply(blocks, function(node) {
    children = xmlChildren(node)
    is.quality = names(children) == "quality"
    info = list(
      data.id = as.integer(xmlValue(children[["did"]])),
      status = xmlValue(children[["status"]]),
      format = xmlValue(children[["format"]]),
      name = xmlValue(children[["name"]])
    )
    qualities = convertNodeSetToList(children[is.quality], fun = as.integer)
    c(info, qualities)
  }), fill = TRUE))
  df$status = factor(df$status, levels = getValidOMLDataSetStatusLevels())

  # subset status level
  ret = droplevels(df[df$status %in% status, , drop = FALSE])
  row.names(ret) = NULL

  names(ret) = convertNamesOMLToR(names(ret))

  return(ret)
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
#' @template arg_number.of.instances
#' @template arg_number.of.features
#' @template arg_number.of.classes
#' @template arg_number.of.missing.values
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
