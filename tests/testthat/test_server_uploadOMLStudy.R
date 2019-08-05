context("uploadOMLStudy")

test_that("uploadOMLStudy", {
  study = makeOMLStudy(alias = "test_alias", name = "Test Upload from R", description = "Just testing")
  expect_class(study, "OMLStudy")
  id = uploadOMLStudy(study)
  expect_is(id, "integer")

  # download study again
  study2 = getOMLStudy(id)
  expect_class(study, "OMLStudy")

  # delete study again
  deleteOMLObject(id, object = "study")
})
