context("download all data sets")

test_that("download all data sets", {
  ids = getOpenMLDatasetNames()

  # remove some simulated data sets. they just take too long as they are large
  j1 = str_detect(ids, "^BNG")
  j2 = str_detect(ids, "^RandomRBF")
  ids = ids[!(j1 | j2)]

  for (i in ids[101:150]) {
    print(i)
    task = downloadOpenMLDataAsMlrTask(i, show.info = FALSE, clean.up = TRUE)
    ds = task$env$data
    expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) > 1 )
  }
})
