#' Run an SQL query on the OpenML DB and retrieve results.
#' 
#' Most users should not need to use this, only a last
#' measure to access all information on the server. 
#' 
#' @param query [\code{character(1)}]\cr
#'   The SQL query.
#' @param simplify [\code{logical(1)}]\cr
#'   If there is only one column in the resulting table, 
#'   directly return the corresponding vector? 
#'   Default is \code{TRUE}.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console? 
#'   Default is \code{FALSE}.
#' @return [\code{data.frame}]. The results as a table.
#' @export
runSQLQuery <- function(query, simplify = TRUE, show.info = FALSE) {
  checkArg(query, "character", len = 1L, na.ok = FALSE)
  checkArg(simplify, "logical", len = 1L, na.ok = FALSE)
  checkArg(show.info, "logical", len = 1L, na.ok = FALSE)

  json.file <- tempfile()

  # replace whitespaces so we dont run into problems with the query
  query <- str_replace_all(query, " ", "%20")

  OPEN_ML_SQL_QUERY_URL <- "http://www.openml.org/api_query"
  URL <- sprintf("%s/?q=%s", OPEN_ML_SQL_QUERY_URL, query)
  download.file(URL, json.file, quiet = TRUE)
  parsed.doc <- fromJSON(file = json.file) 

  if(show.info) 
    message(parsed.doc$status)

  unlink(json.file)

  col.names <- extractSubList(parsed.doc$columns, "title")
  if(is.list(parsed.doc$data) && length(parsed.doc$data) > 1) 
    data <- as.data.frame(t(do.call(cbind, parsed.doc$data)), stringsAsFactors = FALSE)
  else 
    data <- as.data.frame(parsed.doc$data, stringsAsFactors = FALSE)
  colnames(data) <- col.names

  if (ncol(data) == 1L && simplify) 
    return(data[, 1L])
  else 
    return(data)
}


