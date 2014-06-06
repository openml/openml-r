#' Retrieve the names of all available data qualities.
#'
#' @param set [\code{character(1)}]\cr
#'   Either \code{"basic"}, which means only rudimentary data quality names
#'   are retrieved, or \code{"all"}. 
#'   Default is \code{"all"}.
#' @return [\code{character}].
#' @export
getDataQualityNames = function(set = "all") {
  if (set == "all") {
    return(runSQLQuery("SELECT DISTINCT quality FROM data_quality"))
  } else {
    return(c("NumberOfFeatures", "NumberOfInstances", "NumberOfClasses", "MajorityClassSize",
      "MinorityClassSize", "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
      "NumberOfNumericFeatures", "NumberOfSymbolicFeatures"))
  }
}


