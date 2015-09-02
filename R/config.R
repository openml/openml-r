#' @title OpenML configuration.
#'
#' @description
#' After loading the package, it tries to find a configuration in your home
#' directory. The R command \code{path.expand("~/.openml/config")} gives you the
#' full path to the configuration file on your operating system.
#'
#' For further information please read the configuration section in
#' \url{https://github.com/openml/r/wiki}.
#'
#' @name configuration
#' @rdname configuration
#' @family config
NULL

#' @title Loads a config file from disk.
#'
#' @param path [\code{character(1)}]\cr
#'   full path location of the config file to be loaded
#' @return \code{list} of current configuration variables with class \dQuote{OMLConfig}.
#' @family config
#' @export
loadOMLConfig = function(path = "~/.openml/config", assign = TRUE) {
  assertFile(path, access = "r")
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

  return(conf2)
}

# assign element in src conf to dest conf
# if src is NULL, do nothing
assignConfToConf = function(src, dest) {
  if (!is.null(src))
    assertClass(src, "OMLConfig")
  assertClass(dest, "OMLConfig")
  if (is.null(src))
    return()
  lapply(ls(src), function(x) assign(x, src[[x]], envir = dest))
}


# get default entries for all fields in the config, as envir
getDefaultConfig = function() {
  x = as.environment(list(
    server = "http://api_new.openml.org/v1",
    openmldir = path.expand("~/.openml"),
    cachedir = file.path(tempdir(), "cache"),
    verbosity = 1L,
    arff.reader = "RWeka",
    # FIXME: this is the test session hash, we need to remove this soon
    session.hash = "testtest"
  ))
  addClasses(x, "OMLConfig")
}

# check that the config only contains valid entries
# also subtly change some values to a better, standard format
checkConfig = function(conf) {
  ns = ls(conf, all.names = TRUE)
  ns2 = c("openmldir", "server", "session.hash", "cachedir", "verbosity", "arff.reader")
  if (any(ns %nin% ns2))
    stopf("You are only allowed to define the following names in your config:\n%s\nBut you also had:\n%s",
      collapse(ns2, sep = ", "), collapse(setdiff(ns, ns2), sep = ", "))
  assertString(conf$server)
  conf$verbosity = assertInteger(conf$verbosity, lower = 0L, upper = 3L)
  assertString(conf$cachedir)
  assertChoice(conf$arff.reader, c("RWeka", "farff"))
}

#' Set and overwrite configuration settings
#'
#' @param conf [\code{OMLConfig} or \code{list}]\cr
#'   List of configuration parameters as returned by \code{\link{getOMLConfig}}.
#' @param ... [\code{ANY}]\cr
#'   Named configuration parameters. Overwrites parameters in \code{conf}, if provided.
#' @return Invisibly returns a list of configuration settings.
#' @family config
#' @export
setOMLConfig = function(..., conf = NULL) {
  if (!is.null(conf))
    assertClass(conf, "OMLConfig")
  conf2 = addClasses(as.environment(list(...)), "OMLConfig")
  if (!isProperlyNamed(conf2))
    stopf("All configuration arguments in '...' must be properly named")

  conf.cur = getOMLConfig()
  conf.def = getDefaultConfig()

  assignConfToConf(conf, conf.def)
  assignConfToConf(conf2, conf.def)
  checkConfig(conf.def)

  assignConfToConf(conf, conf.cur)
  assignConfToConf(conf2, conf.cur)
  return(conf.cur)
 }

# get a printable string describing the config
printableConfig = function(conf) {
  x = as.list(conf)
  x[setdiff(getConfigNames(), names(x))] = ""
  fmt = paste(
    "OpenML configuration:",
    "  server           : %s",
    "  cachedir         : %s",
    "  verbosity        : %s",
    "  arff.reader      : %s\n",
    sep = "\n")
  expire = ifelse(is.null(x$session.hash.expires), "<not authenticated>",
    as.character(x$session.hash.expires))
  sprintf(fmt, x$server, x$cachedir, expire, x$verbosity, x$arff.reader)
}

#' @export
print.OMLConfig = function(x, ...) {
  cat(printableConfig(x))
}

#' @title Returns a list of OpenML configuration settings.
#'
#' @return \code{list} of current configuration variables with class \dQuote{OMLConfig}.
#' @family config
#' @export
getOMLConfig = function() {
  return(.OpenML.config)
}

