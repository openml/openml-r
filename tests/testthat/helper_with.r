normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache"))
list.files(normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache", "flow")))
list.files(normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache", "flow", "2")))

list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache", "flow")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache", "flow", "2")))

identical(Sys.getenv("TRAVIS"), "true")

with_test_cache = function(expr, envir = parent.frame()) {
  prev = as.list(getOMLConfig())
  on.exit(do.call(setOMLConfig, prev))
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    cachedir = normalizePath(file.path(find.package("OpenML"), "tests", "cache"))
  } else {
    cachedir = normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache"))
  }
  setOMLConfig(cachedir = cachedir)
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
  reset_config({
    setOMLConfig(confirm.upload = FALSE)
    # FIXME: use an API call to check if apikey has write access, see https://github.com/openml/OpenML/issues/267
    # if (identical(Sys.getenv("TRAVIS"), "true")) {
    #   skip_on_travis()
    # } else {
      eval(expr, envir = envir)
    #}
  })
}

with_main_server = function(expr, envir = parent.frame()) {
  reset_config({
    setOMLConfig(server = "http://www.openml.org/api/v1")
    eval(expr, envir = envir)
  })
}

# with_read_only = function(expr, envir = parent.frame()) {
#   reset_config({
#     setOMLConfig(confirm.upload = FALSE)
#     # FIXME: use an API call to check if apikey has write access, see https://github.com/openml/OpenML/issues/267
#     if (identical(Sys.getenv("TRAVIS"), "true")) {
#       eval(expr, envir = envir)
#     } else {
#       skip_on_os(os = c("windows", "mac", "linux", "solaris"))
#     }
#   })
# }
