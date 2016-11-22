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
    server = "http://www.openml.org/api/v1",
    cachedir = normalizePath(file.path(tempdir(), "cache")),
    verbosity = 1L,
    arff.reader = "farff",
    apikey = "PLEASE CHANGE ME",
    confirm.upload = TRUE
  ))
  checkConfig(x)
  addClasses(x, "OMLConfig")
}

# check that the config only contains valid entries
# also subtly change some values to a better, standard format
checkConfig = function(conf) {
  ns = ls(conf, all.names = TRUE)
  ns2 = c("server", "apikey", "cachedir", "verbosity", "arff.reader", "confirm.upload")
  if (any(ns %nin% ns2))
    stopf("You are only allowed to define the following names in your config:\n%s\nBut you also had:\n%s",
      collapse(ns2, sep = ", "), collapse(setdiff(ns, ns2), sep = ", "))
  assertString(conf$server)
  assert(checkChoice(conf$verbosity, 0:2), checkChoice(conf$verbosity, as.character(0:2)))
  conf$verbosity = as.integer(conf$verbosity)
  assertString(conf$cachedir)
  assertString(conf$apikey)
  #assertFlag(conf$confirm.upload)
  if (nchar(conf$apikey) != 32 & conf$apikey != "PLEASE CHANGE ME")
    stopf("The apikey must contain 32 characters, currently it has %i characters", nchar(conf$apikey))
  assertChoice(conf$arff.reader, c("RWeka", "farff"))
}

# get a printable string describing the config
printableConfig = function(conf) {
  if (conf$apikey != "PLEASE CHANGE ME") {
    key = conf$apikey
    substr(key, 1, nchar(key)-5) = collapse(rep("*", nchar(key)-5), "")
  } else {
    key = conf$apikey
  }

  fmt = paste(
    "OpenML configuration:",
    "  server           : %s",
    "  cachedir         : %s",
    "  verbosity        : %s",
    "  arff.reader      : %s",
    "  confirm.upload   : %s",
    "  apikey           : %s\n",
    sep = "\n")
  sprintf(fmt, conf$server, conf$cachedir, conf$verbosity, conf$arff.reader, conf$confirm.upload, key)
}

#' @export
print.OMLConfig = function(x, ...) {
  cat(printableConfig(x))
}
