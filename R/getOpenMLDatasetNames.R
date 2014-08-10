#' @title Get all names of OpenML data sets which are registered on the server.
#'
#' @return [\code{character}].
getOpenMLDatasetNames = function() {
  query = "SELECT name FROM dataset WHERE isOriginal = 'true'"
  res = runSQLQuery(query)
  return(res)
}

