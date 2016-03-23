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
      did = uploadOMLDataSet(ds)
      expect_is(did, "integer")
      deleteOMLObject(did, object = "data")
      
      did = uploadOMLDataSet(mlr.task)
      expect_is(did, "integer")
      deleteOMLObject(did, object = "data")
    })
  })

})
