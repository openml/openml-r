context("listOMLStudies")

test_that("listOMLStudies", {
  with_main_server({
    studies = listOMLStudies()
    expect_data_frame(studies, ncols = 4L, col.names = "unique")
    expect_set_equal(names(studies), c("study.id", "name", "creation.date", "creator"))
    expect_integer(studies$study.id, any.missing = FALSE, all.missing = FALSE)
  })
})
