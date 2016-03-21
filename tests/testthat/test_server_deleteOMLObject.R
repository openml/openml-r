context("deleteOMLObject")

test_that("deleteOMLObject", {
  expect_error(deleteOMLObject(id = 1, object = "run"), "Run is not owned by you")
  expect_error(deleteOMLObject(id = 123456789, object = "run"), "Run does not exists")
})
