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
#' @template arg_showinfo
#' @return [\code{data.frame}]. The results as a table.
#' @export
runSQLQuery = function(query, simplify = TRUE, show.info = getOpenMLOption("show.info")) {
  assertString(query)
  assertFlag(simplify)
  assertFlag(show.info)
  
  json.file = tempfile()
  on.exit(unlink(json.file))
  
  # replace whitespaces so we don't run into problems with the query
  query = str_replace_all(query, " ", "%20")

  # Use new beta server, leave old address commented to be able to switch quickly.
  # query.url = "http://www.openml.org/api_query"
  query.url = "http://openml.liacs.nl/api_query"
  url = sprintf("%s/?q=%s", query.url, query)
  download.file(url, json.file, quiet = TRUE)
  parsed.doc = fromJSON(file = json.file)

  if(show.info)
    message(parsed.doc$status)
 
  data = convertListOfRowsToDataFrame(as.list(parsed.doc$data), strings.as.factors = FALSE,
    col.names = extractSubList(parsed.doc$columns, "title"))
    
  #FIXME: for now guess types, the type is set as undefined in json, everything is encoded as strings
  #data = as.data.frame(lapply(data, type.convert, as.is = TRUE), stringsAsFactors = FALSE)
  
  types = extractSubList(parsed.doc$columns, "datatype")
  if (ncol(data) > 0) {
    for (i in 1:ncol(data)) {
      data[, i] = switch(types[i],
        "int" = as.integer(data[, i]),
        "integer" = as.integer(data[, i]),
        "string" = as.character(data[, i]),
        "double" = as.numeric(data[, i]),
        "blob" = as.character(data[, i]),
        "datetime" = as.character(data[, i]),
        stopf("Unsupported column type: %s", types[i])) 
    }
  }
  
  if (ncol(data) == 1L && simplify)
    return(data[, 1L])
  else
    return(data)
}


