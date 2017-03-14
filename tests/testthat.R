library(testthat)

normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache"))
dir.exists(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache")))

getwd()
list.files(getwd())
normalizePath(file.path(getwd(), ".."))
list.files(normalizePath(file.path(getwd(), "..")))


test_check("OpenML")
