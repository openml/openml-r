context("getOMLRun")

test_that("getOMLRun", {
  run = getOMLRun(1L, get.predictions = TRUE, session.hash)
  expect_is(run, "OMLRun")
  expect_true(run$run.id == 1L)

  expect_error(getOMLRun(id = -1L))

  expect_is(run$predictions, "data.frame")
})
