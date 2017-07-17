context("getOMLStudy")

test_that("getOMLStudy", {
  with_main_server({
    study = getOMLStudy(14)
    expect_is(study, "OMLStudy")
    expect_output(print(study), "study_14")
  })
})
