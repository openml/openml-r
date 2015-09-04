#' @title Saves a list of OpenML configuration settings to file
#'
#' @description The new configuration is automatically assigned via
#' \code{\link{setOMLConfig}} if all checks pass.
#'
#' @param server [\code{character(1)}]\cr
#'   URL of the XML API endpoint.
#' @param verbosity [\code{integer(1)}]\cr
#'   Verbosity level. Possible values are 0 (normal output), 1 (info output),
#'   2(debug output). Default is 1.
#' @param apikey [\code{character(1)}]\cr
#'   Your OpenML API key. Log in to OpenML, move to your profile to get it.
#' @param cachedir [\code{character(1)}]\cr
#'   Path the the cache directory.
#' @param arff.reader [\code{character(1)}]\cr
#'   Name of the package which should be used to parse arff files. Possible are
#'   \dQuote{RWeka}, which is the default and \dQuote{farff}.
#' @param path [\code{character(1)}]\cr
#'   Path to OpenML config file. Default is \dQuote{~/.openml/config}.
#' @param overwrite [\code{logical(1)}]\cr
#'   Should an existing file be overwritten? Default is \code{FALSE}.
#' @family config
#' @export
saveOMLConfig = function(server = NULL, verbosity = NULL,
  apikey = NA, cachedir = NULL, arff.reader = "RWeka", path = "~/.openml/config",
  overwrite = FALSE) {
  assertFlag(overwrite)

  if (file.exists(path) && !overwrite) {
    stopf("Configuration file %s already exists. Set override to TRUE to force overwriting.", path)
  }

  if (!file.exists(dirname(path))) {
    dir.create(dirname(path), showWarnings = TRUE, recursive = TRUE)
  }

  conf = setOMLConfig(server = server, verbosity = verbosity,
    apikey = apikey, cachedir = cachedir,
    arff.reader = arff.reader)
  conf = as.list(conf)
  lines = paste(names(conf), conf, sep = "=")
  con = file(path, "w")
  on.exit(close(con))
  writeLines(lines, con = con)
}
