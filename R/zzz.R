#' @import BBmisc
#' @import checkmate
#' @import digest
#' @import RCurl
#' @import stringr
#' @import XML
#' @import RWeka
#' @import mlr
#' @importFrom data.table rbindlist
#' @importFrom stats setNames
NULL

.OpenML.config = new.env(parent = emptyenv())


if (!exists("SESSION_HASH")) {
  SESSION_HASH = NULL
  SESSION_HASH_EXPIRES = NULL
}

.onAttach = function(libname, pkgname) {
  assignConfigDefaults()
  fn.user = path.expand("~/.openml/config")
  if (!file.exists(fn.user)) {
    packageStartupMessage("openml-r: No configuration found! Using defaults.")
  } else {
    conf = readConfigFile(fn.user)
    assignConfig(conf)
  }
  createCacheSubDirs()
}
