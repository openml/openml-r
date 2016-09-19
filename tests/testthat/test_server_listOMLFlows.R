context("listOMLFlows")

skip_on_cran()

test_that("listOMLFlows", {
  fls = .listOMLFlows(limit = 100L)
  expect_data_frame(fls, nrows = 100L, ncols = 7L, col.names = "unique")
  expect_set_equal(names(fls), c("flow.id", "full.name", "name", "version", "external.version", "uploader", "tags"))
  expect_integer(fls$flow.id, any.missing = FALSE, all.missing = FALSE)

  exp.tag = "study_1"
  fls1 = .listOMLFlows(tag = exp.tag)
  expect_data_frame(fls1, min.rows = 10L, ncols = 7L, col.names = "unique")
  #expect_true(all(grepl(exp.tag, fls1$tags)))
})
