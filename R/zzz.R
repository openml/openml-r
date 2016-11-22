#' @import backports
#' @import BBmisc
#' @import curl
#' @import checkmate
#' @import ParamHelpers
#' @import stringi
#' @import XML
#' @import jsonlite
#' @import data.table
#' @importFrom memoise memoise forget
#' @importFrom digest digest
#' @importFrom stats setNames
#' @importFrom httr POST GET DELETE content upload_file status_code
#' @importFrom stats reshape sd
#' @importFrom utils download.file packageVersion type.convert
NULL

.OpenML.config = getDefaultConfig()

.onLoad = function(libname, pkgname) {
  # set config (especially the cachedir) on package loading, otherwise the cachedir from compile-time will be used
  do.call("setOMLConfig", as.list(getDefaultConfig()))
  # if config file exist, use configuration from this file
  fn.user = path.expand("~/.openml/config")
  if (file.exists(fn.user))
    loadOMLConfig(fn.user, assign = TRUE)
  createCacheSubDirs(verbosity = 0L)
  if (getOMLConfig()$apikey == "PLEASE CHANGE ME")
    message(paste0("Please use the 'setOMLConfig' or 'saveOMLConfig' function to set the API key.\n", 
      "You can generate the API key from your OpenML account at http://www.openml.org/u#!api"))
}
