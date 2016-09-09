library(mlr)
if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("APPVEYOR"), "True")) {
  p = normalizePath("~/.openml/cache", mustWork = FALSE)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  setOMLConfig(apikey = Sys.getenv("OPENMLAPIKEY"), cachedir = p)
}
setOMLConfig(server = "http://test.openml.org/api/v1", confirm.upload = FALSE)
Sys.setenv(NOT_CRAN = "true")
