context("listOMLFlows")

test_that("listOMLFlows", {
  with_main_server({
    fls = .listOMLFlows(limit = 100L)
    expect_data_frame(fls, nrows = 100L, ncols = 6L, col.names = "unique")
    expect_set_equal(names(fls), c("flow.id", "full.name", "name", "version", "external.version", "uploader"))
    expect_integer(fls$flow.id, any.missing = FALSE, all.missing = FALSE)
    
    exp.tag = "study_1"
    fls1 = .listOMLFlows(tag = exp.tag)
    expect_data_frame(fls1, min.rows = 10L, ncols = 6L, col.names = "unique")
  })
})
