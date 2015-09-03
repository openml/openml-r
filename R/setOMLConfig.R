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
    stopf("Argument 'conf' must be of class 'list' or 'OMLConfig', not %s", head(conf, 1L))
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
  invisible(addClasses(conf, "OMLConfig"))
}
