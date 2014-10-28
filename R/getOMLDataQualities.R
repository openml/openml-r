#' Retrieve various data quality measures that describe a data set.
#'
#' Useful if you want to get a first impression of the available data sets or select
#' appropriate data sets for a certain study.
#'
#' For a complete list of all data qualities, see \code{\link{getOMLDataQualityNames}}.
#'
#' @param set [\code{character(1)}]\cr
#'   Either \code{"basic"}, which means only rudimentary data qualities (number of
#'   features/instances/classes/missing values/...) are retrieved, or \code{"all"}.
#'   The latter includes 'basic' data qualities as well as meta learning features.
#'   Default is \code{"basic"}.
#'
#' @return [\code{data.frame}]. Rows correspond to data sets, columns to data qualities.
#' @export
getOMLDataQualities = function(set = "basic") {
  # FIXME: use API call
  assertChoice(set, c("all", "basic"))

  dquals = if (set == "all")
    getOMLDataQualityNames()
  else
    c("NumberOfFeatures", "NumberOfInstances", "NumberOfClasses", "MajorityClassSize",
      "MinorityClassSize", "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
      "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")

  # we need to recode missing values in qualities a bit by applying an SQL function for each quality
  query = sprintf("MAX(IF(dq.quality='%s', dq.value, NULL)) AS `%s`", dquals, dquals)
  query = collapse(query)
  query = paste("SELECT d.did AS did, d.name AS dataset, d.version, ", query,
    "FROM dataset d, data_quality dq WHERE d.did = dq.data AND d.isOriginal = 'true' GROUP BY did")

  dq = runSQLQuery(query)
  types = runSQLQuery("SELECT name, datatype FROM quality")

  # iterate over data qualities and convert their types. First three columns are did, dataset and
  # version, ignore them here.
  for (i in 4:ncol(dq)) {
    type = types[types$name == colnames(dq)[i], "datatype"]
    if (length(type) == 0L)
      type = "undefined"
    dq[, i] = switch(type,
      "integer" = as.integer(dq[, i]),
      "double" = as.numeric(dq[, i]),
      "string" = as.character(dq[, i]),
      type.convert(as.character(dq[, i]), as.is = TRUE))
  }
  rownames(dq) = dq$did
  return(dq)
}
