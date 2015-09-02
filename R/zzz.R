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

.OpenML.config = addClasses(new.env(parent = emptyenv()), "OMLConfig")

.onLoad = function(libname, pkgname) {
  # assignConfigDefaults()
  # fn.user = path.expand("~/.openml/config")
  # if (file.exists(fn.user))
    # assignConfig(readConfigFile(fn.user))
  # FIXME: this is probably forbidden on cran?
  # createCacheSubDirs(verbosity = FALSE)
}

