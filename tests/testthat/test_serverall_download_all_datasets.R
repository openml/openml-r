context("download all data sets")

# we download "most" safe data sets, because large ones might cause runtime and mem problems currently

test_that("download all data sets", {
  skip_on_cran()
  skip_on_travis()

  dsl = listOMLDataSets()
  dids = dsl[dsl$status == "active" & dsl$NumberOfInstances <= 10000 & dsl$NumberOfFeatures <= 100, "did"]
  # fix for private data sets
  dids = dids[!is.na(dids)]

  for (id in dids) {
    print(id)
    oml.data = getOMLDataSet(id)
    expect_is(oml.data, "OMLDataSet")
    expect_is(oml.data$desc, "OMLDataSetDescription")
    expect_identical(length(oml.data$colnames.new), length(oml.data$colnames.old))
    ds = oml.data$data
    expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) >= 1)
  }
})
