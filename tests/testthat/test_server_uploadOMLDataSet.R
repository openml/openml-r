context("uploadOMLDataSet")

test_that("uploadOMLDataSet", {
  # download a run and reupload it
  with_test_cache({
    ds = getOMLDataSet(10L)
    mlr.task = convertOMLDataSetToMlr(ds)

    # with_read_only({
    #   expect_error(uploadOMLDataSet(ds), "This is a read-only account")
    #   expect_error(uploadOMLDataSet(mlr.task), "This is a read-only account")
    # })

    with_write_access({
      data.id = uploadOMLDataSet(ds)
      expect_is(data.id, "integer")
      deleteOMLObject(data.id, object = "data")

      data.id = uploadOMLDataSet(mlr.task)
      expect_is(data.id, "integer")
      deleteOMLObject(data.id, object = "data")
      
      # upload multilabel
      ds$desc$target.features = ds$desc$default.target.attribute = c("bl_of_lymph_c", "bl_of_lymph_s")
      ds$data$bl_of_lymph_c = as.logical(as.numeric(ds$data$bl_of_lymph_c)-1)
      ds$data$bl_of_lymph_s = as.logical(as.numeric(ds$data$bl_of_lymph_s)-1)
      data.id = uploadOMLDataSet(ds)
      expect_is(data.id, "integer")
      deleteOMLObject(data.id, object = "data")
    })
  })

})
