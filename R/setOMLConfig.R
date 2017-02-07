#' @title Settter for configuration settings.
#'
#' @description
#' Set and overwrite configuration settings.
#'
#' @param server [\code{character(1)}]\cr
#'   URL of the XML API endpoint.
#' @param verbosity [\code{integer(1)}]\cr
#'   Verbosity level. Possible values are 0 (normal output), 1 (info output),
#'   2 (debug output).
#' @param apikey [\code{character(1)}]\cr
#'   Your OpenML API key. Log in to OpenML, move to your profile to get it.
#' @param cachedir [\code{character(1)}]\cr
#'   Path to the cache directory.
#' @param arff.reader [\code{character(1)}]\cr
#'   Name of the package which should be used to parse arff files. Possible are
#'   \dQuote{RWeka}, which is the default and \dQuote{farff}.
#' @param confirm.upload [\code{logical(1)}]\cr
#'   Should the user be asked for confirmation before upload of OML objects?
#' @param download.method [\code{cahracter(1)}]\cr
#'   Method used to download files. See \code{method} argument in \code{download.file}
#' @return Invisibly returns a list of configuration settings.
#' @family config
#' @export
setOMLConfig = function(server = NULL, verbosity = NULL,
  apikey = NULL, cachedir = NULL, arff.reader = NULL,
  confirm.upload = NULL, download.method = NULL) {

  if (is.null(server))
    server = getOMLConfig()$server
  else
    assertString(server)
  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  else
    verbosity = asInt(verbosity)
  if (is.null(apikey))
    apikey = getOMLConfig()$apikey
  else
    assertString(apikey)
  if (is.null(cachedir))
    cachedir = getOMLConfig()$cachedir
  else
    assertString(cachedir)
  if (is.null(server))
    server = getOMLConfig()$server
  else
    assertString(server)
  if (is.null(arff.reader))
    arff.reader = getOMLConfig()$arff.reader
  else
    assertChoice(arff.reader, c("RWeka", "farff"))
  if (is.null(confirm.upload))
    confirm.upload = getOMLConfig()$confirm.upload
  else
    confirm.upload = as.logical(confirm.upload)
  if (is.null(download.method))
    download.method = getOMLConfig()$download.method
  else
    assertChoice(download.method, c("auto", "internal", "wininet", "libcurl", "wget", "curl"))

  conf = list(server = server, verbosity = verbosity, apikey = apikey,
    cachedir = cachedir, arff.reader = arff.reader, confirm.upload = confirm.upload, 
    download.method = download.method)
  conf2 = addClasses(as.environment(conf), "OMLConfig")
  checkConfig(conf2)

  conf.cur = getOMLConfig()
  checkConfig(conf.cur)
  assignConfToConf(conf2, conf.cur)

  return(conf.cur)
}
