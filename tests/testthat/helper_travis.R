if (identical(Sys.getenv("TRAVIS"), "true")) {
  p = normalizePath("~/.openml/cache", mustWork = FALSE)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  setOMLConfig(apikey = Sys.getenv("OPENMLAPIKEY"), cachedir = p)
}
