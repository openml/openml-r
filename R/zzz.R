#' @import BBmisc
#' @import digest
#' @import RCurl
#' @import rjson
#' @import stringr
#' @import XML
#' @import RWeka
#' @import mlr
#' @import checkmate
#' @importFrom data.table rbindlist
#' @importFrom stats setNames
NULL

.OpenML.config = new.env(parent = emptyenv())
class(.OpenML.config) = c("OMLConfig", class(.OpenML.config))

.onAttach = function(libname, pkgname) {
  readConfigAndAssign()
  createCacheSubDirs()
}
