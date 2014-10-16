#' @import BBmisc
#' @import digest
#' @import RCurl 
#' @import rjson 
#' @import stringr 
#' @import XML
#' @import RWeka
#' @import mlr
#' @import plyr
#' @import checkmate
#' @import base64enc

.onAttach = function(libname, pkgname) {
  configureOpenML()
}