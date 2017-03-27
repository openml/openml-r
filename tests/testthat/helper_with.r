reset_config = function(expr, envir = parent.frame()) {
  prev = as.list(getOMLConfig())
  on.exit({
    do.call(setOMLConfig, prev)
    do.call(saveOMLConfig, c(prev, overwrite = TRUE))
  })
  eval(expr, envir = envir)
}

with_test_cache = function(expr, envir = parent.frame()) {
  reset_config({
    if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("APPVEYOR"), "True")) {
      cachedir = normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache"))
    } else {
      cachedir = normalizePath(file.path(find.package("OpenML"), "tests", "cache"))
    }
    setOMLConfig(cachedir = cachedir)
    eval(expr, envir = envir)
  })
}

with_empty_cache = function(expr, envir = parent.frame()) {
  reset_config({
    dir = tempfile()
    dir.create(dir, recursive = TRUE)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    setOMLConfig(cachedir = dir)
    eval(expr, envir = envir)
  })
}

with_main_server = function(expr, envir = parent.frame()) {
  reset_config({
    setOMLConfig(server = "http://www.openml.org/api/v1")
    eval(expr, envir = envir)
  })
}
