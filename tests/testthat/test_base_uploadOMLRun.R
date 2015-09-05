context("uploadOMLRun")

test_that("uploadOMLRun", {
  # FIXME: reenable test
  task = getOMLTask(1)
  lrn = makeLearner("classif.rpart")
  run = runTaskMlr(task, lrn)
  flow.id = uploadOMLFlow(lrn)
  expect_is(flow.id, "integer")
  run$flow.id = flow.id
  run.id = uploadOMLRun(run)
  expect_is(run.id, "integer")
  deleteOMLObject(run.id, object = "run")
  deleteOMLObject(flow.id, object = "flow")
})
