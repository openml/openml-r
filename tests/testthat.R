library(testthat)
library(OpenML)
normalizePath(file.path(find.package("OpenML"), "tests", "cache"))
dir.exists(normalizePath(file.path(find.package("OpenML"), "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "tests", "cache", "flows")))

test_check("OpenML")
