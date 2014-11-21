context("getOMLRun")

test_that("getOMLRun", {
  run = getOMLRun(1L, session.hash)
  expect_is(run, "OMLRun")
  expect_true(run$run.id == 1L)

  expect_error(getOMLRun(id = -1L))

  preds = getOMLPredictions(run, session.hash)
  expect_is(preds, "data.frame")
})
