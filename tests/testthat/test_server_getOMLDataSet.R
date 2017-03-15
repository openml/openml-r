context("getOMLDataSet")

test_that("getOMLDataSet by name", {
  # Note: this is a server test, since querying a data set by name requires a
  # call to listOMLDataSets
  with_empty_cache({
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
  })
})
