#' @title Construct OMLDataSet.
#'
#' @param desc [\code{\link{OMLDataSetDescription}}]\cr
#'   The data set's description.
#' @param data [\code{data.frame}]\cr
#'   The data set.
#' @param colnames.old [\code{character}]\cr
#'   The original column names of the data set. These might not be valid names in R!
#' @param colnames.new [\code{character}]\cr
#'   New column names that are actually used in R. These are valid and unique and differ from the
#'   original column names only if those are invalid.
#' @export
#' @aliases OMLDataSet
makeOMLDataSet = function(desc, data, colnames.old, colnames.new) {
  assertClass(desc, "OMLDataSetDescription")
  assertDataFrame(data)
  assertCharacter(colnames.old, any.missing = FALSE)
  assertCharacter(colnames.old, any.missing = FALSE)

  makeS3Obj("OMLDataSet",
    desc = desc, data = data, colnames.old = colnames.old, colnames.new = colnames.new)
}

# ***** Methods *****

#' @export
print.OMLDataSet = function(x, ...) {
  print.OMLDataSetDescription(x$desc)
}
