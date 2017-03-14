library(testthat)

list.files(normalizePath(file.path(find.package("OpenML"))))
list.files(normalizePath(file.path(find.package("OpenML"), "..")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache", "flows")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache", "flows", "2")))

test_check("OpenML")

list.files(normalizePath(file.path(find.package("OpenML"))))
list.files(normalizePath(file.path(find.package("OpenML"), "..")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache", "flows")))
list.files(normalizePath(file.path(find.package("OpenML"), "..", "tests", "cache", "flows", "2")))
