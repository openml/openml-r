context("listOMLRunEvaluations")

test_that("listOMLRunEvaluations", {
  task.id = 3L
  run = listOMLRuns(task.id = task.id)

  # filter only successful runs
  run = run[is.na(run$error.message), , drop = FALSE]
  run.evals = listOMLRunEvaluations(task.id = task.id)
  expect_is(run.evals, "data.frame")
  expect_true(isSubset(run.evals$run.id, run$run.id))

  # now check stuff for runs
  run.ids = 1:100
  runs = listOMLRuns(run.id = run.ids)
  run.evals = listOMLRunEvaluations(run.id = run.ids)

  # subset only runs without error
  runs = runs[is.na(runs$error.message),]
  run.id = unique(runs$run.id)
  setup.id = unique(runs$setup.id)
  flow.id = unique(runs$flow.id)
  uploader.id = unique(runs$uploader)

  expect_equal(sort(runs$run.id), sort(run.evals$run.id))

  # for (i in c("run.id", "setup.id", "flow.id", "uploader.id")) {
  #   id = get(i)[length(get(i))]
  #   if (i == "uploader.id") i = "uploader"
  #   rl = do.call("listOMLRunEvaluations", setNames(list(id), i))
  #   expect_true(all(rl[, i] == id))
  # }
})
