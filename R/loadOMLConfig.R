#' @title Load OpenML configuration.
#'
#' @description Loads the OpenML config file from the disk and overwrites the
#' current OpenML config.
#'
#' @param path [\code{character(1)}]\cr
#'   Full path location of the config file to be loaded.
#' @param assign [\code{logical(1)}]\cr
#'   Use the loaded configuration as the current configuration?
#'   If set to \code{FALSE}, the configuration is just returned by the function.
#'   Default is \code{TRUE}.
#' @return \code{list} of current configuration variables with class \dQuote{OMLConfig}.
#' @family config
#' @export
#' @example inst/examples/loadOMLConfig.R
loadOMLConfig = function(path = "~/.openml/config", assign = TRUE) {
  assertFile(path, access = "r")
  assertFlag(assign)
  conf = new.env(parent = emptyenv())

  # get all lines, trimmed, and remove empty lines
  lines = Filter(nzchar, stri_trim_both(readLines(path)))

  # check: the format is <name> = <value>
  pattern.check = stri_match(lines, regex = "\\s*\\S+\\s*=\\s*\\S+\\s*")
  pattern.check = !is.na(pattern.check[, 1L])
  first.err = which.first(!pattern.check)
  if (length(first.err) > 0L)
    stopf("You have a format error in your config in line:\n%s", lines[first.err])

  # get names and values from config, convert to named evir
  lines = stri_split_fixed(lines, "=")
  lines = lapply(lines, stri_trim_both)
  lines = do.call(rbind, lines)
  conf = as.environment(setNames(as.list(lines[, 2L]), lines[, 1L]))
  conf = addClasses(conf, "OMLConfig")

  # contruct default config envir, assign the parsed values into it, then check
  conf2 = getDefaultConfig()
  assignConfToConf(conf, conf2)
  checkConfig(conf2)

  if (assign) {
    assignConfToConf(conf2, getOMLConfig())
  }

  return(conf2)
}
