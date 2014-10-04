context("download all data sets")

# we download "most" safe data sets, because large ones might cause runtime and mem problems currently

test_that("download all data sets", {
  quals = getDataQualities()
  dsets = getOpenMLDatasetNames()
  quals2 = subset(quals, dataset %in% dsets$name & NumberOfInstances <= 10000 & 
    NumberOfFeatures <= 100 & dataset != "Zoo_test_jan")

  ids = quals2[, c("dataset", "version")]
  
  for (i in 1:nrow(ids)) {
    if (ids[i, "version"] %in% dsets[dsets$name == ids[i, "dataset"], ]) {
      print(sprintf("%s, v. %i", ids[i, "dataset"], ids[i, "version"]))
      oml.data = downloadOpenMLData(ids[i, "dataset"], version = ids[i, "version"], 
                                    show.info = FALSE, clean.up = TRUE)
      expect_true(oml.data$name == ids[i, "dataset"])
      expect_true(oml.data$version == ids[i, "version"])
      ds = oml.data$data.set
      expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) >= 1)
    }
  }
})
