#' Retrieve various data quality measures that describe a data set.
#'
#' Useful if you want to get a first impression of the available data sets or select
#' appropriate data sets for a certain study.
#'
#' For a complete list of all data qualities, see \code{\link{getDataQualityNames}}.
#'
#' @param set [\code{character(1)}]\cr
#'   Either \code{"basic"}, which means only rudimentary data qualities (number of features/instances/classes/missing values/...)
#'   are retrieved, or \code{"all"}. The latter includes 'basic' data qualities as well as meta learning features. 
#'   Default is \code{"basic"}.
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
  return(res)
}


