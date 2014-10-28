context("download all data sets")

# we download "most" safe data sets, because large ones might cause runtime and mem problems currently

test_that("download all data sets", {
  skip_on_cran()
  skip_on_travis()
  
  dsl = getOMLDataSetList(session.hash = session.hash)
  dids = dsl[dsl$status == "active" & dsl$NumberOfInstances <= 10000 & dsl$NumberOfFeatures <= 100, "did"]
  
  for (id in dids) {
    print(id)
    oml.data = downloadOMLDataSet(id, session.hash)
    expect_is(oml.data, "OMLDataSet")
    expect_is(oml.data$desc, "OMLDataSetDescription")
    expect_identical(length(oml.data$colnames.new), length(oml.data$colnames.old))
    ds = oml.data$data
    expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) >= 1)
  }
})
