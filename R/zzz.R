#' @import BBmisc
#' @import digest
#' @import RCurl
#' @import rjson
#' @import stringr
#' @import XML
#' @import RWeka
#' @import mlr
#' @import checkmate
#' @import base64enc
#' @importFrom stats setNames
#' @importFrom plyr rbind.fill
NULL

.OpenML.config = new.env()
class(.OpenML.config) = c("OMLConfig", class(.OpenML.config))

.onAttach = function(libname, pkgname) {
  readConfigAndAssign()
  createCacheSubDirs()
}



