# FIXME: add to namespace
library(rjson)

#' Make an SQL query.
#' 
#' Retrieve the results of an SQL query. 
#' 
#' @param SQL [\code{character(1)}]\cr
#'   The SQL query in form of a character string.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console? 
#'   Default is \code{TRUE}.
#' @return [\code{data.frame}]. A \code{data.frame} containing the results.
#' @export
openMLSQLQuery <- function(SQL, show.info = TRUE) {
  json.file <- tempfile()
  
  OPEN_ML_SQL_QUERY_URL <- "http://www.openml.org/api_query"
  URL <- sprintf("%s/?q=%s", OPEN_ML_SQL_QUERY_URL, SQL)
  download.file(URL, json.file, quiet = TRUE)
  parsed.doc <- fromJSON(file = json.file) 
  
  if(show.info) {
    message(parsed.doc$status)
  }
   
  unlink(json.file)
  
  col.names <- lapply(parsed.doc$columns, function(x) x$title)
  if(is.list(parsed.doc$data) && length(parsed.doc$data) > 1) {
    data <- as.data.frame(t(do.call(cbind, parsed.doc$data)))
  } else {
    data <- as.data.frame(parsed.doc$data)
  }
  colnames(data) <- col.names
  
  return(data)
}

#' getDataQualityNames
#' 
#' Retrieve the names of all available data qualities. 
#' 
#' @return [\code{character}]. A \code{character vector} containing the data quality names.
#' @export
getDataQualityNames <- function() {
  as.character(openMLSQLQuery("SELECT DISTINCT quality FROM data_quality")[, 1])
}

#' getMetaLearningFeatures
#' 
#' Retrieve meta learning features about all stored data sets. Basic data characteristics as 
#' obtained by \code{\link{getDataCharacteristics}} are also included.
#' 
#' @param only [\code{character}]\cr
#'   The data qualities that are to be retrieved. A complete list can be obtained by \code{\link{getDataQualityNames}}.
#'   Default is all data qualities are retrieved.
#' 
#' @return [\code{data.frame}]. A \code{data.frame} containing the data qualities.
#' @export
getMetaLearningFeatures <- function(only = NULL) {
  if(missing(only)) {
    only <- getDataQualityNames()
  }
  
  # FIXME: remove the splitting of 'only' if it's possible to download an unlimited amount of qualities at the same time
  if(length(only) > 24) {
    qualities.part1 <- getDataQualities(only = only[1:24])
    only <- only[25:length(only)]
  }
  
  SQL.query <- "SELECT d.name"
  for(i in only) {
    SQL.query <- sprintf("%s, MAX(IF(dq.quality='%s', dq.value, 0)) AS %s", SQL.query, i, i)
  }
  SQL.query <- 
    sprintf(
      "%s FROM dataset d, data_quality dq WHERE d.did = dq.data AND d.isOriginal = 'true' GROUP BY d.name", 
      SQL.query)
  qualities <- openMLSQLQuery(SQL.query)
  
  if(exists("qualities.part1")) {
    qualities <- cbind(qualities.part1, qualities[, -1])
  }
  return(qualities)
}

#' Get basic data characteristics.
#' 
#' Retrieve the following "non-meta" data characteristics:  
#' "NumberOfFeatures", "NumberOfInstances", "NumberOfClasses", 
#' "NumberOfInstancesWithMissingValues", "NumberOfMissingValues", "NumberOfNumericFeatures", 
#' "NumberOfSymbolicFeatures"
#' 
#' @return [\code{data.frame}]. A \code{data.frame} containing the data characteristics.
#' @export
getDataCharacteristics <- function() {
  chars <- c("NumberOfFeatures", "NumberOfInstances", "NumberOfClasses", "MajorityClassSize", 
    "MinorityClassSize", "NumberOfInstancesWithMissingValues", "NumberOfMissingValues", 
    "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")
  data.chars <- getMetaLearningFeatures(only = chars)
  # all features (except for the name) are integers. [check this when adding new data chars]
  data.chars[, -1] <- apply(data.chars[, -1], 2, as.integer)
  return(data.chars)
}
