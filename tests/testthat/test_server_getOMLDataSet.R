test_that("getOMLDataSet by name", {
  # Note: this is a server test, since querying a data set by name requires a
  # call to listOMLDataSets
  with_empty_cache({
    with_main_server({
      # fail if both name and ID is provided
      expect_error(getOMLDataSet(data.id = 42, data.name = "iris"))

      # get newest version
      data = getOMLDataSet(data.name = "iris")
      checkOMLDataSet(data)

      # get invalid version
      expect_error(getOMLDataSet(data.name = "iris", data.version = 835389), "Available versions")

      # get first version
      data = getOMLDataSet(data.name = "iris", data.version = 1)
      checkOMLDataSet(data)

      # get deactivated and in_preparation dataset
      ds = listOMLDataSets(status = "deactivated", limit = 1)
      expect_warning(getOMLDataSet(ds$data.id), "Data set has been deactivated.")
      ds = listOMLDataSets(status = "in_preparation", limit = 1)
      expect_warning(getOMLDataSet(ds$data.id), "Data set is in preparation and will be activated soon.")

      # check option
      expect_error(getOMLDataSet(1479, cache.only = TRUE), "not found in cache with option")
      data = getOMLDataSet(1479)
      checkOMLDataSet(data)

      # check multilabel
      multilab.ds = listOMLDataSets(tag = "2016_multilabel_r_benchmark_paper", limit = 1)
      ds = getOMLDataSet(data.id = multilab.ds$data.id)
      expect_atomic_vector(ds$target.features, min.len = 2)

      # check if there are no warnings in case of multiple elements in ignore.attributes (see issue #439)
      expect_silent(getOMLTask(146800, verbosity = 0))
    })
  })
})
