context("uploadOMLFlow")

test_that("uploadOMLFlow", {
  # download a flow and reupload it
  flow = getOMLFlow(1699)
  expect_is(flow, "OMLFlow")
  expect_is(flow$flow.id, "integer")
  # FIXME: sourcefiles and/or binaryfiles should be automatically set 
  # (downloaded from flow$source.url) if they are available
  
  flow.id = uploadOMLFlow(flow, sourcefile = flow$source.path)
  # upload it again
  expect_message(uploadOMLFlow(flow, sourcefile = flow$source.path), "Flow already exists")
 
  # create a own flow
  lrn = makeLearner("classif.rpart")
  flow.id = uploadOMLFlow(lrn)
  expect_is(flow.id, "integer")
})
