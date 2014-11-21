context("listOMLFlows")

test_that("listOMLFlows", {
  fl = listOMLFlows(session.hash)
  expect_is(fl, "data.frame")
  expect_true(nrow(fl) > 100L && ncol(fl) == 6L)
  expect_true(setequal(names(fl), c("id", "full.name", "name", "version", "external.version", "uploader")))
  expect_true(is.integer(fl$id) && !any(is.na(fl$id)))
})
