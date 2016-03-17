context(".listOMLRunEvaluations/server")

test_that(".listOMLRunEvaluations", {
  task.id = 3L
  runs = .listOMLRuns(task.id = 3L)

  # filter only successful runs
  run.evals = .listOMLRunEvaluations(task.id = task.id)
  expect_data_frame(run.evals, min.rows = 1L, col.names = "unique")
  expect_subset(run.evals$run.id, runs$run.id)
})
