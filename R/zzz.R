#' @import BBmisc
#' @import checkmate
#' @import ParamHelpers
#' @import stringi
#' @import XML
#' @import mlr
#' @import data.table
#' @importFrom digest digest
#' @importFrom stats setNames
#' @importFrom httr POST GET DELETE content upload_file status_code
NULL

.OpenML.config = getDefaultConfig()

.onLoad = function(libname, pkgname) {
  # assignConfigDefaults()
  fn.user = path.expand("~/.openml/config")
  if (file.exists(fn.user))
    loadOMLConfig(fn.user, assign = TRUE)
  # FIXME: this is probably forbidden on cran?
  createCacheSubDirs(verbosity = FALSE)
}
