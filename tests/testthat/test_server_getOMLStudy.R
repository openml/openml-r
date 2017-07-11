context("getOMLStudy")

test_that("getOMLStudy", {
  with_main_server({
    study = getOMLStudy(14)
    expect_is(study, "OMLStudy")
    expect_output(print(study), "study_14")

    data.ids = getOMLBenchmarkDataSetIds()
    expect_set_equal(data.ids, study$data$data.id)

    task.ids = getOMLBenchmarkTaskIds()
    expect_set_equal(task.ids, study$tasks$task.id)
  })
})
