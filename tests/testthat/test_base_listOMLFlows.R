context("listOMLFlows")

test_that("listOMLFlows", {
  fl = listOMLFlows()
  expect_is(fl, "data.frame")
  expect_true(nrow(fl) > 100L && ncol(fl) == 6L)
  expect_true(setequal(names(fl), c("flow.id", "full.name", "name", "version", "external.version", "uploader")))
  expect_true(is.integer(fl$flow.id) && !any(is.na(fl$flow.id)))
})
