context("listOMLRunEvaluations")

test_that("listOMLRunEvaluations", {
  with_main_server({
    task.id = 3832L
    runs = .listOMLRuns(task.id = task.id)

    # filter only successful runs
    run.evals = .listOMLRunEvaluations(task.id = task.id, evaluation.measure = "area_under_roc_curve")
    expect_data_frame(run.evals, min.rows = 1L, col.names = "unique")
    expect_subset(run.evals$run.id, runs$run.id)
    expect_subset(c("run.id", "task.id", "setup.id", "flow.id", "flow.name", "flow.source", "data.name"),
      names(run.evals))

    # filter runs with tag: study_1
    run.evals2 = .listOMLRunEvaluations(task.id = task.id, tag = "study_1", evaluation.measure = "area_under_roc_curve")
    expect_data_frame(run.evals2, min.rows = 1L, col.names = "unique")
    expect_subset(run.evals2$run.id, run.evals$run.id)

    run.evals3 = .listOMLRunEvaluations(task.id = 3953, tag = "study_1", evaluation.measure = "area_under_roc_curve")
    run.evals4 = .listOMLRunEvaluations(task.id = c(task.id, 3953), tag = "study_1", evaluation.measure = "area_under_roc_curve")
    expect_equal(nrow(run.evals4), nrow(run.evals2) + nrow(run.evals3))

    # try evaluation.measure arg
    # run.eval.meas = listOMLRunEvaluations(task.id = task.id, evaluation.measure = "area_under_roc_curve")
    # expect_equal(run.eval.meas$area.under.roc.curve, run.evals$area.under.roc.curve)
    # expect_true(ncol(run.evals) > ncol(run.eval.meas))

    # try wrong evaluation measure
    expect_error(listOMLRunEvaluations(task.id = task.id, evaluation.measure = "m"))
  })
})
