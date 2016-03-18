context("uploadOMLDataSet")

test_that("uploadOMLDataSet", {
  # download a run and reupload it
  ds = getOMLDataSet(1L)
  did = uploadOMLDataSet(ds)
  expect_is(did, "integer")
  deleteOMLObject(did, object = "data")

  mlr.task = convertOMLDataSetToMlr(ds)
  did = uploadOMLDataSet(mlr.task)
  expect_is(did, "integer")
  deleteOMLObject(did, object = "data")
})
