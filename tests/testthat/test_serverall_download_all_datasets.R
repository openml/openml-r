context("download all data sets")

test_that("download all data sets", {
  ids = getOpenMLDatasetNames()
  for (i in ids) {
    print(i)
    task = downloadOpenMLDataAsMlrTask(i, show.info = FALSE, clean.up = TRUE)
    ds = task$env$data
    expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) > 1 )
  }
})
