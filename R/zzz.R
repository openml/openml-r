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
#' @importFrom plyr rbind.fill
NULL

.onAttach = function(libname, pkgname) {
  configureOpenML(
    show.info = getOpenMLOption("show.info", TRUE),
    cache.dir = getOpenMLOption("cache.dir", tempdir()),
    cache.compression = getOpenMLOption("cache.compression", "none")
  )
}
