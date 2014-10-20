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

.onAttach = function(libname, pkgname) {
  setOpenMLOption("show.info", getOpenMLOption("show.info", TRUE))
  setOpenMLOption("cache.dir", getOpenMLOption("cache.dir", tempdir()))
}
