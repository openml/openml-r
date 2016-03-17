with_test_cache = function(expr, envir = parent.frame()) {
  prev = as.list(getOMLConfig())
  on.exit(do.call(setOMLConfig, prev))
  setOMLConfig(cachedir = file.path(find.package("OpenML"), "tests", "cache"), apikey = strrep("0", 32))
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

regen_test_cache = function() {
  with_test_cache({
    getOMLTask(task.id = 59)
  })
}
