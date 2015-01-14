context("uploadOMLRun")

test_that("uploadOMLRun", {
  task = getOMLTask(1)
  lrn = makeLearner("classif.J48")
  run = runTaskMlr(task, lrn)
  impl.id = uploadOMLFlow(lrn)
  run.id = uploadOMLRun(run, impl.id)
  expect_is(run.id, "integer")
  deleteOMLRun(run.id)
})
