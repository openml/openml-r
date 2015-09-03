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
