library(testthat)

normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache"))
list.files(normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache", "flow")))
list.files(normalizePath(file.path(find.package("OpenML"), "00_pkg_src", "OpenML", "tests", "cache", "flow", "2")))

list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache", "flow")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache", "flow", "2")))

identical(Sys.getenv("TRAVIS"), "true")

test_check("OpenML")
