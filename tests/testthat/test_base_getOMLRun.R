context("getOMLRun")

test_that("getOMLRun", {
  run = getOMLRun(1L, get.predictions = TRUE)
  expect_is(run, "OMLRun")
  expect_true(run$run.id == 1L)

  expect_error(getOMLRun(run.id = -1L))
  expect_error(getOMLRun(run.id = 1464351321L), "Run not found")

  expect_is(run$predictions, "data.frame")
})
