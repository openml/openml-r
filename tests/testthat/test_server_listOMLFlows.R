context("listOMLFlows")

skip_on_cran()

test_that("listOMLFlows", {
  fls = .listOMLFlows()
  expect_data_frame(fls, min.rows = 100L, ncols = 6L, col.names = "unique")
  expect_set_equal(names(fls), c("flow.id", "full.name", "name", "version", "external.version", "uploader"))
  expect_integer(fls$flow.id, any.missing = FALSE, all.missing = FALSE)
  
  fls1 = .listOMLFlows(tag = "study_1")
  expect_true(nrow(fls) > nrow(fls1))
})
