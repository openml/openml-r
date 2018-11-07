context("uploadOMLDataSet")

test_that("uploadOMLDataSet", {
  # download a run and reupload it
  with_test_cache({
    ds = getOMLDataSet(10L)
    mlr.task = convertOMLDataSetToMlr(ds)

    data.id = uploadOMLDataSet(ds)
    expect_is(data.id, "integer")
    deleteOMLObject(data.id, object = "data")

    data.id = uploadOMLDataSet(mlr.task)
    expect_is(data.id, "integer")
    deleteOMLObject(data.id, object = "data")

    # upload multilabel
    ds$desc$target.features = ds$desc$default.target.attribute = c("bl_of_lymph_c", "bl_of_lymph_s")
    ds$data$bl_of_lymph_c = as.logical(as.numeric(ds$data$bl_of_lymph_c) - 1)
    ds$data$bl_of_lymph_s = as.logical(as.numeric(ds$data$bl_of_lymph_s) - 1)
    data.id = uploadOMLDataSet(ds)
    expect_is(data.id, "integer")
    deleteOMLObject(data.id, object = "data")

    # upload data with multiple ignore attributes
    iris = getTaskData(iris.task)
    desc = makeOMLDataSetDescription(
      name = "iris",
      description = "iris with ignored features Sepal.Width and Petal.Length",
      ignore.attribute = c("Sepal.Width", "Petal.Length"),
      default.target.attribute = "Species"
    )
    d = makeOMLDataSet(desc, data = iris)
    did = uploadOMLDataSet(d)
    d2 = getOMLDataSet(did)
    expect_is(convertOMLDataSetToMlr(d2), "Task")
  })
})
