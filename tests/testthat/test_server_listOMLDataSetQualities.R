context("listOMLDataSetQualities")

skip_on_cran()

test_that("listOMLDataSetQualities", {
  dsqs = listOMLDataSetQualities()
  expect_data_frame(dsqs, min.rows = 64, ncols = 1)
  expect_set_equal(names(dsqs), "name")
  expect_character(dsqs$name, any.missing = FALSE, all.missing = FALSE)
})
