#' Loads a config file from disk to mem
#'
#' @param path [\code{character(1)}]\cr
#'   full path location of the config file to be loaded
#' @return \code{list} of current configuration variables with class \dQuote{OMLConfig}.
#' @family config
#' @export
loadOMLConfig = function(path = "~/.openml/config") {
  assertFile(path)
  # read and assign config file
  conf = readConfigFile(path.expand(path))
  assignConfig(conf)
  # create cache dir from new config file
  createCacheSubDirs(verbosity = FALSE)
  invisible(conf)
}
