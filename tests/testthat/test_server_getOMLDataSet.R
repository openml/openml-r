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
  })
})
