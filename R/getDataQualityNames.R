#' Retrieve the names of all available data qualities.
#'
#' @return [\code{character}].
#' @export
getDataQualityNames <- function() {
  runSQLQuery("SELECT DISTINCT quality FROM data_quality")
}


