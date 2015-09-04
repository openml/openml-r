context("uploadOMLRun")

test_that("uploadOMLRun", {
  # FIXME: reenable test
  task = getOMLTask(1)
  lrn = makeLearner("classif.rpart")
  run = runTaskMlr(task, lrn)
  flow.id = uploadOMLFlow(lrn)
  run$flow.id = flow.id
  run.id = uploadOMLRun(run)
  expect_is(run.id, "integer")
  deleteOMLRun(run.id)
})
