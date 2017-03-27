context("listOMLRunEvaluations")

test_that("listOMLRunEvaluations", {
  with_main_server({
    task.id = 3L
    runs = .listOMLRuns(task.id = task.id)
    
    # filter only successful runs
    run.evals = .listOMLRunEvaluations(task.id = task.id)
    expect_data_frame(run.evals, min.rows = 1L, col.names = "unique")
    expect_subset(run.evals$run.id, runs$run.id)
    expect_subset(c("run.id", "task.id", "setup.id", "flow.id", "flow.name", "flow.source", "data.name"), 
      names(run.evals))
    
    # filter runs with tag: study_1
    run.evals2 = .listOMLRunEvaluations(task.id = 3L, tag = "study_1")
    expect_data_frame(run.evals2, min.rows = 1L, col.names = "unique")
    expect_subset(run.evals2$run.id, run.evals$run.id)
    
    run.evals3 = .listOMLRunEvaluations(task.id = 2, tag = "study_1")
    run.evals4 = .listOMLRunEvaluations(task.id = 2:3, tag = "study_1")
    expect_equal(nrow(run.evals4), nrow(run.evals2) + nrow(run.evals3))
  })
})
