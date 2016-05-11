context("listOMLRuns")

skip_on_cran()

test_that("listOMLRuns", {
  exp.names = c("run.id", "task.id", "setup.id", "flow.id", "uploader", "error.message")

  rs = .listOMLRuns(task.id = 2L)
  expect_data_frame(rs, ncols = 6L, min.rows = 50, col.names = "unique")
  expect_true(all(rs$task.id == 2L))
  expect_set_equal(names(rs), exp.names)
  
  # filter runs with tag: study_1
  rs2 = .listOMLRuns(task.id = 2L, tag = "study_1")
  rs3 = .listOMLRuns(task.id = 3L, tag = "study_1")
  rs4 = .listOMLRunEvaluations(task.id = 2:3, tag = "study_1")
  expect_equal(nrow(rs4), nrow(rs2) + nrow(rs3))
})
