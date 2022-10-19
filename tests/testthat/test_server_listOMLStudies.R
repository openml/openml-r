test_that("listOMLStudies", {
  with_main_server({
    studies = .listOMLStudies()
    expect_data_frame(studies, ncols = 3L, col.names = "unique")
    expect_set_equal(names(studies), c("id", "alias", "name"))
    expect_integer(studies$id, any.missing = FALSE, all.missing = FALSE)
  })
})
