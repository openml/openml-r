with_test_cache = function(expr, envir = parent.frame()) {
  prev = as.list(getOMLConfig())
  on.exit(do.call(setOMLConfig, prev))
  setOMLConfig(cachedir = file.path(find.package("OpenML"), "tests", "cache"))
  eval(expr, envir = envir)
}

with_empty_cache = function(expr, envir = parent.frame()) {
  prev = as.list(getOMLConfig())
  on.exit(do.call(setOMLConfig, prev))
  dir = tempfile()
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  setOMLConfig(cachedir = dir)
  eval(expr, envir = envir)
}

reset_config = function(expr, envir = parent.frame()) {
  prev = as.list(getOMLConfig())
  on.exit({
    do.call(setOMLConfig, prev)
    do.call(saveOMLConfig, c(prev, overwrite = TRUE))
  })
  eval(expr, envir = envir)
}

with_write_access = function(expr, envir = parent.frame()) {
  # FIXME: use an API call to check if apikey has write access, see https://github.com/openml/OpenML/issues/267
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    skip_on_travis()
  } else {
    eval(expr, envir = envir)
  }
}

with_read_only = function(expr, envir = parent.frame()) {
  # FIXME: use an API call to check if apikey has write access, see https://github.com/openml/OpenML/issues/267
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    eval(expr, envir = envir)
  } else {
    skip_on_os(os = c("windows", "mac", "linux", "solaris"))
  }
}
