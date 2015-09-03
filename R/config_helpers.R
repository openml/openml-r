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
    apikey = "testtest"
  ))
  addClasses(x, "OMLConfig")
}

# check that the config only contains valid entries
# also subtly change some values to a better, standard format
checkConfig = function(conf) {
  ns = ls(conf, all.names = TRUE)
  ns2 = c("openmldir", "server", "apikey", "cachedir", "verbosity", "arff.reader")
  if (any(ns %nin% ns2))
    stopf("You are only allowed to define the following names in your config:\n%s\nBut you also had:\n%s",
      collapse(ns2, sep = ", "), collapse(setdiff(ns, ns2), sep = ", "))
  assertString(conf$server)
  assert(checkChoice(conf$verbosity, 0:3), checkChoice(conf$verbosity, as.character(0:3)))
  conf$verbosity = as.integer(conf$verbosity)
  assertString(conf$cachedir)
  assertChoice(conf$arff.reader, c("RWeka", "farff"))
}

# get a printable string describing the config
printableConfig = function(conf) {
  fmt = paste(
    "OpenML configuration:",
    "  server           : %s",
    "  cachedir         : %s",
    "  verbosity        : %s",
    "  arff.reader      : %s\n",
    sep = "\n")
  sprintf(fmt, conf$server, conf$cachedir, conf$verbosity, conf$arff.reader)
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
