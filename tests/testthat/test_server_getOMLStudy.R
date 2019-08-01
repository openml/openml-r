context("getOMLStudy")

test_that("getOMLStudy", {
  with_main_server({
    study = .getOMLStudy(99)
    study.by.alias = .getOMLStudy("OpenML-CC18")

    expect_is(study, "OMLStudy")
    expect_output(print(study), "OpenML-CC18|99")

    expect_equal(study, study.by.alias)

    context("extractOMLStudyIds")
    test_that("extractOMLStudyIds", {
    expect_set_equal(extractOMLStudyIds(study, type = "data.id"),
      study$data$data.id)
    expect_set_equal(extractOMLStudyIds(study, type = "task.id"),
      study$tasks$task.id)
    expect_set_equal(extractOMLStudyIds(study, type = "flow.id"),
      study$flows$flow.id)
    expect_set_equal(extractOMLStudyIds(study, type = "run.id"),
      study$runs$run.id)
    })
  })
})
