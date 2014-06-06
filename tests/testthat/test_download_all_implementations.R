# FIXME do this on CRAN?
context("download all implementations")

test_that("download all implementations", {
  impls = as.character(runSQLQuery("SELECT id FROM implementation"))
  #FIXME remove or define complete test as external or whatever
  errs = c()
  for (i in seq_along(impls)) {
    id = impls[i]
    print(id)
    res = try({
      impl = downloadOpenMLImplementation(id = id, download.source.binary = FALSE, clean.up = TRUE)
    })
    if (is.error(res)) {
      errs = c(errs, id)
    }
  }
  #FIXME remove
  xxx <= errs
  print("Errors:")
  print(errs)
})  