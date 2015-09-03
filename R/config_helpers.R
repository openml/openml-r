#' @export
print.OMLConfig = function(x, ...) {
  cat(printableConfig(x))
}

# sources config file and returns the envir
readConfigFile = function(conffile) {
  assertFile(conffile)

  conf = new.env(parent = emptyenv())
  # FIXME: we need to check the format of the conf file
  # the format is <name> = <value>
  lines = Filter(nzchar, stri_trim_both(readLines(conffile)))
  lines = stri_split_fixed(lines, "=")
  lines = lapply(lines, stri_trim_both)
  lines = do.call(rbind, lines)
  conf = as.environment(setNames(as.list(lines[, 2L]), lines[, 1L]))
  # if (is.error(x))
    # stopf("There was an error in sourcing your configuration file '%s': %s!", conffile, as.character(x))

  if (!is.null(conf$verbosity))
    conf$verbosity = as.integer(conf$verbosity)
  if (!is.null(conf$openmldir))
    conf$openmldir = path.expand(conf$openmldir)
  if (!is.null(conf$cachedir))
    conf$cachedir = path.expand(conf$cachedir)
  if (!is.null(conf$arff.reader))
    requireNamespace(conf$arff.reader) else
      requireNamespace("RWeka")

  checkConfig(conf)
  # FIXME: probably horrible security wise....
  if (!is.null(conf$password))
    conf$pwdmd5 = digest(conf$password, serialize = FALSE)
  conf$password = NULL

  conf = addClasses(conf, "OMLConfig")
  return(conf)
}

# assigns a conf to namespace
assignConfig = function(conf) {
  conf.in.ns = getOMLConfig()
  conf.in.ns$is.user.config = TRUE
  invisible(lapply(ls(conf), function(x) assign(x, conf[[x]], envir = conf.in.ns)))
}

assignConfigDefaults = function() {
  conf = getOMLConfig()
  conf$server = "http://api_new.openml.org/v1"
  conf$username = NA_character_
  conf$pwdmd5 = NA_character_
  conf$openmldir = path.expand("~/.openml")
  conf$cachedir = file.path(tempdir(), "cache")
  conf$verbosity = 1L
  conf$arff.reader = "RWeka"
  conf$is.user.config = FALSE
  conf$session.hash = "testtest"
}

getConfigNames = function() {
  c("server", "username", "password", "cachedir", "verbosity", "arff.reader")
}

checkConfig = function(conf) {
  ns = if (is.list(conf)) names(conf) else ls(conf, all.names = TRUE)
  ns2 = getConfigNames()
  if (any(ns %nin% ns2))
    stopf("You are only allowed to define the following R variables in your config:\n%s\nBut you also had:\n%s",
      collapse(ns2, sep = ", "), collapse(setdiff(ns, ns2), sep = ", "))
  if (!is.null(conf$server))
    assertString(conf$server)
  if (!is.null(conf$username))
    assertString(conf$username)
  if (!is.null(conf$password))
    assertString(conf$password)
  if (!is.null(conf$verbosity))
    verbosity = asInt(conf$verbosity)
  if (!is.null(conf$cachedir))
    assertString(conf$cachedir)
  if (!is.null(conf$arff.reader))
    assertString(conf$arff.reader)
}

# Function which returns a printable string describing the config
# Used in packageStartupMessage and in print.Config
printableConfig = function(conf) {
  x = as.list(conf)
  x[setdiff(getConfigNames(), names(x))] = ""
  fmt = paste(
    "OpenML configuration:",
    "  server           : %s",
    "  username         : %s",
    "  cachedir         : %s",
    "  pwdmd5           : ***",
    "  session expires  : %s",
    "  verbosity        : %s",
    "  arff.reader      : %s\n",
    sep = "\n")
  expire = ifelse(is.null(x$session.hash.expires), "<not authenticated>",
    as.character(x$session.hash.expires))
  sprintf(fmt, x$server, x$username, x$cachedir, expire, x$verbosity, x$arff.reader)
}
