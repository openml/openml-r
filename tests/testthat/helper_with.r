with_test_cache = function(expr, envir = parent.frame()) {
  prev = as.list(getOMLConfig())
  on.exit(do.call(setOMLConfig, prev))
  setOMLConfig(cachedir = file.path(find.package("OpenML"), "tests", "cache"))
  eval(expr, envir = envir)
}

with_no_cache = function(expr, envir = parent.frame()) {
  prev = as.list(getOMLConfig())
  on.exit(do.call(setOMLConfig, prev))
  setOMLConfig(cachedir = tempfile())
  eval(expr, envir = envir)
}
