test_that("listOMLDataSetQualities", {
  with_main_server({
    dsqs = .listOMLDataSetQualities()
    expect_data_frame(dsqs, min.rows = 64, ncols = 1)
    expect_set_equal(names(dsqs), "name")
    expect_character(dsqs$name, any.missing = FALSE, all.missing = FALSE)
  })
})
