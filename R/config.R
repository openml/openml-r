#' OpenML configuration.
#'
#' In order to understand how the package should be configured
#' please read
#' \url{https://github.com/tudo-r/BatchJobs/wiki/Configuration}.
#'
#' @name configuration
#' @rdname configuration
#' @family conf
NULL

# sources config file and returns the envir
readConfigFile = function(conffile) {
  assertFile(conffile)

  conf = new.env()
  # FIXME: we need to check the format of the conf file
  # the format is <name> = <value>
  lines = readLines(conffile)
  lines = str_trim(lines)
  lines = Filter(function(x) nchar(x) > 0L, lines)
  lines = str_split(lines, "=")
  lines = lapply(lines, str_trim)
  lines = do.call(rbind, lines)
  conf = as.environment(setNames(as.list(lines[, 2L]), lines[, 1L]))
  # if (is.error(x))
    # stopf("There was an error in sourcing your configuration file '%s': %s!", conffile, as.character(x))
  
  if (!is.null(conf$verbosity))
    conf$verbosity = as.integer(conf$verbosity)
  
  checkConfig(conf)
  # FIXME: probably horrible security wise....
  if (!is.null(conf$password))
    conf$pwdmd5 = digest(conf$password, serialize = FALSE)
  conf$password = NULL
  
  conf = addClasses(conf, "OpenMLConfig")
  return(conf)
}

# assigns a conf to namespace
assignConfig = function(conf) {
  conf.in.ns = getOpenMLConfig()
  lapply(ls(conf), function(x) assign(x, conf[[x]], envir = conf.in.ns))
}

# reads package conf, userhome conf, working dir conf
# then assigns them to namespace
readConfigAndAssign = function() {
  assignConfigDefaults()
  fn.user = path.expand("~/.openml/config")
  if (!file.exists(fn.user)) {
    warning("No configuration found! Assigning defaults.")
  } else {
    conf = readConfigFile(fn.user)
    assignConfig(conf)
  }
}

assignConfigDefaults = function() {
  conf = getOpenMLConfig()
  conf$server = "http://www.openml.org"
  conf$username = NA_character_
  conf$pwdmd5 = NA_character_
  conf$openmldir = path.expand("~/.openml")
  conf$cachedir = file.path(conf$openmldir, "cache")
  conf$verbosity = 1L
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
    "  server   : %s",
    "  username : %s",
    "  cachedir : %s",
    "  pwdmd5   : ***",
    "  verbosity: %s\n",
    sep = "\n")
  sprintf(fmt, x$server, x$username, x$cachedir, x$verbosity)
}


#' @export
print.OpenMLConfig = function(x, ...) {
  cat(printableConfig(x))
}

#' Set and overwrite configuration settings
#'
#' @param conf [\code{OpenMLConfig} or \code{list}]\cr
#'   List of configuration parameters as returned by \code{\link{loadConfig}} or \code{\link{getConfig}}.
#' @param ... [\code{ANY}]\cr
#'   Named configuration parameters. Overwrites parameters in \code{conf}, if provided.
#' @return Invisibly returns a list of configuration settings.
#' @family conf
#' @export
setOpenMLConfig = function(conf = list(), ...) {
  if (!is.list(conf) && !inherits(conf, "OpenMLConfig"))
    stopf("Argument 'conf' must be of class 'list' or 'Config', not %s", head(conf, 1L))
  overwrites = insert(conf, list(...))
  if (length(overwrites) == 0L)
    return(invisible(getConfig()))
  if (!isProperlyNamed(overwrites))
    stopf("All configuration arguments in '...' must be properly named")
  checkConfig(overwrites)
  conf = insert(as.list(getOpenMLConf()), overwrites)
  assignConfig(as.environment(conf))
  invisible(setClasses(conf, "Config"))
}

#' Returns a list of OpenML configuration settings
#'
#' @return \code{list} of current configuration variables with classs \dQuote{OpenMLConfig}.
#' @family conf
#' @export
getOpenMLConfig = function() {
  get(".OpenML.config", envir = getNamespace("OpenML"))
}

