context("getOMLStudy")

test_that("getOMLStudy", {
  with_main_server({
    study = getOMLStudy(14)
    study.by.alias = getOMLStudy("OpenML100")

    expect_is(study, "OMLStudy")
    expect_output(print(study), "OpenML100|14")

    expect_equal(study, study.by.alias)
  })
})
