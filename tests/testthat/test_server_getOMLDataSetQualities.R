context("getOMLDataSetQualities")

test_that("getOMLDataSetQualities", {
  with_test_server({
    qual = getOMLDataSetQualities(1)
    expect_data_frame(qual, min.rows = 1L, ncol = 2L)
    expect_set_equal(names(qual), c("name", "value"))
    expect_character(qual$name, unique = TRUE, any.missing = FALSE)
    expect_numeric(qual$value, any.missing = FALSE)
  })
})
