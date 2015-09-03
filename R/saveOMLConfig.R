#' @title Saves a list of OpenML configuration settings to file
#'
#' @param conf [\code{list}]\cr
#'   Named list of configuration values.
#' @param ... [any]\cr
#'   Another possibility to pass configuration options.
#' @param path [\code{character(1)}]\cr
#'   Path to OpenML config file. Default is \dQuote{~/.openml/config}.
#' @param overwrite [\code{logical(1)}]\cr
#'   Should an existing file be overwritten? Default is \code{FALSE}.
#' @family config
#' @export
saveOMLConfig = function(conf = list(), ..., path = "~/.openml/config", overwrite = FALSE) {
  assertFlag(overwrite)
  conf = insert(conf, list(...))
  checkConfig(conf)

  if (file.exists(path) && !overwrite) {
    stopf("Configuration file %s already exists. Set override to TRUE to force overwriting.", path)
  }

  lines = paste(names(conf), conf, sep = "=")
  print(lines)
  con = file(path, "w")
  on.exit(close(con))
  writeLines(lines, con = con)
}
