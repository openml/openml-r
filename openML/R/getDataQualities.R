#' Retrieve various data quality measures that describe a data set.
#'
#' Useful if you want to get a first impression of the available data sets or select
#' appropriate data sets for a certain study.
#' Retrieve meta learning features about all stored data sets. Basic data characteristics as
#' obtained by \code{\link{getDataCharacteristics}} are also included.
#'
#' @param only [\code{character}]\cr
#'   The data qualities that are to be retrieved. A complete list can be obtained by \code{\link{getDataQualityNames}}.
#'   Default is all data qualities are retrieved.
#'
#' @return [\code{data.frame}]. Rows correspond to data sets, columns to data qualities.
#' @export
getDataQualities <- function(set = "basic") {
  checkArg(set, choices = c("basic", "all"))

  dquals = if (set == "all")
    getDataQualityNames()
  else
    c("NumberOfFeatures", "NumberOfInstances", "NumberOfClasses", "MajorityClassSize",
      "MinorityClassSize", "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
      "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")

  # we need to recode missing values in qualities a bit by applying an SQL function for each quality
  query <- sprintf("MAX(IF(dq.quality='%s', dq.value, NULL)) AS %s", dquals, dquals)
  query <- collapse(query)
  query <- paste("SELECT d.name AS dataset,", query,
    "FROM dataset d, data_quality dq WHERE d.did = dq.data AND d.isOriginal = 'true' GROUP BY dataset")

  res <- runSQLQuery(query)
  #FIXME: for now guess types, the type is set as undefined in json, everything is encoded as strings
  res <- as.data.frame(lapply(res, type.convert, as.is = TRUE), stringsAsFactors = FALSE)

  return(res)
}


