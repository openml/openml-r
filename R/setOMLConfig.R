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
