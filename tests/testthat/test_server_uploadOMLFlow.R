context("uploadOMLFlow")

test_that("uploadOMLFlow", {
  with_empty_cache({
    # get a flow
    flow = getOMLFlow(2794)
    expect_is(flow, "OMLFlow")
    expect_is(flow$flow.id, "integer")
    # FIXME: sourcefiles and/or binaryfiles should be automatically set 
    # (downloaded from flow$source.url) if they are available
    
    # create a own flow
    lrn = makeLearner("classif.rpart")
    lrnW = makeFilterWrapper(makeImputeWrapper(lrn, classes = list(numeric = imputeMedian(), integer = imputeMedian())), fw.perc = 0.5, fw.method = "variance")
    
    # with_read_only({
    #   expect_error(uploadOMLFlow(flow, sourcefile = flow$source.path), "This is a read-only account")
    #   expect_error(uploadOMLFlow(lrn), "This is a read-only account")
    # })
    
    with_write_access({
      flow.id = uploadOMLFlow(flow, sourcefile = flow$source.path)
      expect_is(flow.id, "integer")
      expect_message(uploadOMLFlow(flow, sourcefile = flow$source.path), "Flow already exists")
      #deleteOMLObject(flow.id, object = "flow")
      
      flow.id = uploadOMLFlow(lrn)
      expect_is(flow.id, "integer")
      expect_message(uploadOMLFlow(lrn), "Flow already exists")
      #deleteOMLObject(flow.id, object = "flow")
      
      # upload wrapped learner
      flow.id = uploadOMLFlow(lrnW)
      expect_is(flow.id, "integer")
      expect_message(uploadOMLFlow(lrnW), "Flow already exists")
      
      # upload tune wrapper
    })
  })
})
