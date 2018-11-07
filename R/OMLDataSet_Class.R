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
#'   If set, this will replace the default target in \code{desc}.
#' @return [\code{OMLDataSet}]
#' @name OMLDataSet
#' @export
#' @family data set-related functions
#' @aliases OMLDataSet
#' @example inst/examples/makeOMLDataSet.R
makeOMLDataSet = function(desc, data, colnames.old = colnames(data), colnames.new = colnames(data), target.features = NULL) {
  # sanity check for desc
  assertClass(desc, "OMLDataSetDescription")
  #assertSubset(desc$default.target.attribute, choices = c(colnames(data), NA), empty.ok = TRUE)
  #assertSubset(desc$ignore.attribute, choices = c(colnames(data), NA), empty.ok = TRUE)
  #assertSubset(desc$row.id.attribute, choices = c(colnames(data), NA), empty.ok = TRUE)
  assertDataFrame(data)
  assertCharacter(colnames.old, len = ncol(data), any.missing = FALSE, all.missing = FALSE)
  assertCharacter(colnames.new, len = ncol(data), any.missing = FALSE, all.missing = FALSE)
  assertSubset(target.features, choices = colnames(data), empty.ok = TRUE)
  # use default target if no target is passed, else replace default target with passed target
  if (is.null(target.features)) {
    target.features = desc$default.target.attribute
  } else {
    if (any(desc$default.target.attribute != target.features)) {
      verbosity = getOMLConfig()$verbosity
      showInfo(verbosity, sprintf("Default target will be replaced with '%s'.", collapse(target.features)))
    }
    desc$default.target.attribute = target.features
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
