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
class(.OpenML.config) = "OMLConfig"
SESSION_HASH = NULL
SESSION_EXPIRE = NULL

.onAttach = function(libname, pkgname) {
  readConfigAndAssign()
  createCacheSubDirs()
}

