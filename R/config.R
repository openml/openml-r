.OpenML.config = new.env(parent = emptyenv())

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

# sources config file and returns the envir
readConfigFile = function(conffile) {
  assertFile(conffile)

  conf = new.env(parent = emptyenv())
  # FIXME: we need to check the format of the conf file
  # the format is <name> = <value>
  lines = Filter(nzchar, str_trim(readLines(conffile)))
  lines = str_split(lines, "=")
  lines = lapply(lines, str_trim)
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
  conf$server = "http://www.openml.org"
  conf$username = NA_character_
  conf$pwdmd5 = NA_character_
  conf$openmldir = path.expand("~/.openml")
  conf$cachedir = file.path(tempdir(), "cache")
  conf$verbosity = 1L
  conf$is.user.config = FALSE
}

getConfigNames = function() {
  c("server", "username", "password", "cachedir", "verbosity")
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
    "  verbosity        : %s\n",
    sep = "\n")
  expire = ifelse(is.null(x$session.hash.expires), "<not authenticated>",
    as.character(x$session.hash.expires))
  sprintf(fmt, x$server, x$username, x$cachedir, expire, x$verbosity)
}


#' @export
print.OMLConfig = function(x, ...) {
  cat(printableConfig(x))
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
setOMLConfig = function(conf = list(), ...) {
  if (!is.list(conf) && !inherits(conf, "OMLConfig"))
    stopf("Argument 'conf' must be of class 'list' or 'Config', not %s", head(conf, 1L))
  if (!is.null(conf$openmldir))
    conf$openmldir = path.expand(conf$openmldir)
  if (!is.null(conf$cachedir))
    conf$cachedir = path.expand(conf$cachedir)
  overwrites = insert(conf, list(...))
  if (length(overwrites) == 0L)
    return(invisible(getOMLConfig()))
  if (!isProperlyNamed(overwrites))
    stopf("All configuration arguments in '...' must be properly named")
  checkConfig(overwrites)
  conf = insert(as.list(getOMLConfig()), overwrites)
  assignConfig(as.environment(conf))
  invisible(setClasses(conf, "Config"))
}

#' Returns a list of OpenML configuration settings
#'
#' @return \code{list} of current configuration variables with class \dQuote{OMLConfig}.
#' @family config
#' @export
getOMLConfig = function() {
  addClasses(.OpenML.config, "OMLConfig")
}

#' Loads a config file from disk to mem 
#'
#' @param path [\code{character(1)}]\cr 
#'   full path location of the config file to be loaded
#' @return invisible(NULL)
#' @family config
#' @export
loadOMLConf = function(path = "~/.openml/config") {
  assertFile(path)
  assignConfig(readConfigFile(path.expand(path)))
  createCacheSubDirs(verbosity = FALSE)
  invisible(NULL)
}