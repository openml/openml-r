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

  parsed.doc = fromJSON(file = sprintf("%s/?q=%s", OPENML_URL_API_QUERY, URLencode(query)))

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
