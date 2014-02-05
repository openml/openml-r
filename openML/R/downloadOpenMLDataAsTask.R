#' Download an OpenML data set and convert it into an mlr task.
#'
#'
#' @param name [\code{character}]\cr
#'   The name of the data set.
#' @param dir [\code{character}]\cr
#'   The directory where intermediate files are stored. If clean.up = TRUE, this does not matter.
#'   Default is the session's temporary directory.
#' @param clean.up [\code{loigcal}]\cr
#'   Should the downloaded files be removed from disk at the end?
#'   Default is \code{TRUE}.
#' @param show.info [\code{logical(1)}]\cr
#'   Verbose output on console?
#'   Default is \code{TRUE}.
#' @return [\code{\link[mlr]{SupervisedTask}}]
#' @export
downloadOpenMLDataAsMlrTask <- function(name, dir = tempdir(), clean.up = TRUE, show.info = TRUE) {
  checkArg(name, "character", len = 1L, na.ok = FALSE)
  checkArg(dir, "character", len = 1L, na.ok = FALSE)
  checkArg(clean.up, "logical", len = 1L, na.ok = FALSE)
  checkArg(show.info, "logical", len = 1L, na.ok = FALSE)

  fn.data.set.desc <- file.path(dir, "data_set_description.xml")
  fn.data.set <- file.path(dir, "data_set.ARFF")

  # get id for given dataset name
  query <- paste0("SELECT did, default_target_attribute FROM dataset WHERE name = '", name, "'")
  res <- runSQLQuery(query)
  if (nrow(res) == 0L)
    stopf("No data set on OpenML server found for: %s", name)
  id <- res[1L, "did"]
  target <- res[1,"default_target_attribute"]

  if (show.info) {
    messagef("Downloading data set '%s' from OpenML repository.", name)
    messagef("Intermediate files (XML and ARFF) will be stored in : %s", dir)
  }

  downloadOpenMLDataSetDescription(id = id, file = fn.data.set.desc, show.info = show.info)
  task.data.desc <- parseOpenMLDataSetDescription(file = fn.data.set.desc)
  downloadOpenMLDataSet(task.data.desc@url, fn.data.set, show.info)
  task.data.desc@data.set <- parseOpenMLDataSet(task.data.desc, fn.data.set)

  if (clean.up) {
    unlink(fn.data.set.desc)
    unlink(fn.data.set)
    if (show.info)
      messagef("All intermediate XML and ARFF files are now removed.")
  }

  # FIXME: Dirty from now on:
  classif <- is.factor(task.data.desc@data.set[, target])

  data <- task.data.desc@data.set
  # FIXME: some data sets have empty factor levels, mlr does not like this
  # fix this for now by removing
  data <- droplevels(data)

  # FIXME: hack to convert bad feature names
  feature.ind <- which(colnames(data) %nin% target)
  feature.names <- colnames(data)[feature.ind]
  feature.names <- str_replace_all(feature.names, pattern=c("\\-"), replacement="_")
  feature.names <- str_replace_all(feature.names, pattern=c("/"), replacement="_")
  colnames(data)[feature.ind] <- feature.names

  if (classif) {
    mlr.task <- makeClassifTask(id = name, data = data, target = target)
  } else {
    mlr.task <- makeRegrTask(id = name, data = data, target = target)
  }

  return(mlr.task)
}
