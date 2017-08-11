context("getOMLBenchmarkSuite")

test_that("getOMLBenchmarkSuite", {
  with_main_server({
    study = getOMLStudy(14)
    task.ids = getOMLBenchmarkSuite(name = "OpenML100")
    expect_set_equal(task.ids, study$task$task.id)
    expect_error(getOMLBenchmarkSuite(name = "mysuite"))
  })
})
