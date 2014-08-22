context("download all data sets")

# we download "most" safe data sets, because large ones might cause runtime and mem problems currently

test_that("download all data sets", {
  quals = getDataQualities()
  dsets = getOpenMLDatasetNames()
  quals2 = subset(quals, dataset %in% dsets$name & NumberOfInstances <= 10000 & NumberOfFeatures <= 100)

  ids = quals2$dataset

  for (i in ids) {
    print(i)
    task = downloadOpenMLDataAsMlrTask(i, show.info = FALSE, clean.up = TRUE)
    ds = task$env$data
    expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) > 1 )
  }
})
