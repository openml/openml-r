#' @title OMLDataSet.
#'
#' @description
#' An \code{OMLDataSet} consists of an \code{OMLDataSetDescription}, a
#' \code{data.frame} containing the data set, the old and new column names and,
#' finally, the target features.
#'
#' The \code{\link{OMLDataSetDescription}} provides information on the data set,
#' like the ID, name, version, etc. To see a full list of all elements, please see the
#' \href{https://github.com/openml/website/blob/master/openml_OS/views/pages/api_new/v1/xsd/openml.data.upload.xsd}{XSD}.
#'
#' The slot \code{colnames.old} contains the original names, i.e., the column names that were
#' uploaded to the server, while \code{colnames.new} contains the names that you will see when
#' working with the data in R.
#' Most of the time, old and new column names are identical. Only if the original names are
#' not valid, the new ones will differ.
#'
#' The slot \code{target.features} contains the column name(s) from the \code{data.frame}
#' of the \code{OMLDataSet} that refer to the target feature(s).
#'
#' @param desc [\code{OMLDataSetDescription}]\cr
#'   Data set description.
#' @param data [\code{data.frame}]\cr
#'   The data set.
#' @param colnames.old [\code{character}]\cr
#'   Names of the features that were uploaded to the server.
#' @param colnames.new [\code{character}]\cr
#'   Names of the features that are displayed.
#' @param target.features [\code{character}]\cr
#'   Name(s) of the target feature(s).
#' @return [\code{OMLDataSet}]
#' @name OMLDataSet
#' @export
#' @family data set-related functions
#' @aliases OMLDataSet
#' @example inst/examples/makeOMLDataSet.R
makeOMLDataSet = function(desc, data, colnames.old = colnames(data), colnames.new = colnames(data), target.features) {
  assertClass(desc, "OMLDataSetDescription")
  assertDataFrame(data)
  n.col = ncol(data)
  assertCharacter(colnames.old, len = n.col, any.missing = FALSE, all.missing = FALSE)
  assertCharacter(colnames.new, len = n.col, any.missing = FALSE, all.missing = FALSE)
  assertCharacter(target.features, min.len = 0L, max.len = n.col, any.missing = TRUE, all.missing = TRUE)

  if (!isSubset(target.features, colnames(data))) {
    stopf("Data has no column(s) named '%s'.", collapse(setdiff(target.features, colnames(data)), sep = ", "))
  }

  makeS3Obj("OMLDataSet",
    desc = desc,
    data = data,
    colnames.old = colnames.old,
    colnames.new = colnames.new,
    target.features = target.features
  )
}

#' @export
print.OMLDataSet = function(x, ...) {
  print.OMLDataSetDescription(x$desc)
}

#' @export
as.data.frame.OMLDataSet = function(x, ...) {
  x$data
}

#' @export
as.data.table.OMLDataSet = function(x, ...) {
  as.data.table(x$data)
}
