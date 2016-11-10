#' @import backports
#' @import BBmisc
#' @import curl
#' @import checkmate
#' @import ParamHelpers
#' @import stringi
#' @import XML
#' @import jsonlite
#' @import xml2
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
  # assignConfigDefaults()
  fn.user = path.expand("~/.openml/config")
  if (file.exists(fn.user))
    loadOMLConfig(fn.user, assign = TRUE)
  # FIXME: this is probably forbidden on cran?
  createCacheSubDirs(verbosity = 0L)
}
