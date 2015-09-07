context("uploadOMLRun")

test_that("uploadOMLRun", {
  # download a run and reupload it
  run = getOMLRun(1)
  expect_is(run, "OMLRun")
  expect_is(run$flow.id, "integer")
  expect_is(run$run.id, "integer")
  expect_is(run$task.id, "integer")
  maxrun = max(listOMLRuns(task.id = run$task.id)$run.id)
  run.id = uploadOMLRun(run)
  expect_is(run.id, "integer")
  expect_true(maxrun < run.id)
  run$flow.id = NA
  expect_error(uploadOMLRun(run), "Please provide an 'flow.id'")
})
