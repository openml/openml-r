#' @import BBmisc
#' @import checkmate
#' @import digest
#' @import RCurl
#' @import stringi
#' @import XML
#' @import mlr
#' @import data.table
#' @importFrom stats setNames
NULL

.onLoad = function(libname, pkgname) {
  assignConfigDefaults()
  fn.user = path.expand("~/.openml/config")
  if (file.exists(fn.user))
    assignConfig(readConfigFile(fn.user))
  createCacheSubDirs(verbosity = FALSE)
}

.onAttach = function(libname, pkgname) {
  conf = getOMLConfig()
  if (!conf$is.user.config)
    packageStartupMessage("OpenML: No configuration found! Using defaults.")
}
