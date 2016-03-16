context("getOMLRun")

test_that("getOMLRun", {
  clearOMLCache()
  expect_error(getOMLRun(1L, cache.only = TRUE), "not found in cache")

  run = getOMLRun(1L)
  expect_output(print(run), "OpenML Run")
  expect_output(print(run, print.metrics = TRUE), "OpenML Run")
  expect_true(!is.na(run$flow.name))

  expect_is(run, "OMLRun")
  expect_true(run$run.id == 1L)

  # check IO data
  run.IOdata = run$output.data
  expect_is(run.IOdata, "OMLIOData")
  expect_output(print(run.IOdata), "Data Sets")

  # check parameter settings (get first parameter only)
  param.setting = run$parameter.setting[[1L]]
  param.setting$component = "Component" # just to trigger the codeblock
  expect_is(param.setting, "OMLRunParameter")
  expect_output(print(param.setting), "Component")

  expect_error(getOMLRun(run.id = -1L))
  expect_error(getOMLRun(run.id = 1464351321L), "Run not found")

  expect_is(run$predictions, "data.frame")

  run = vector("list", length(unlist(task.clean)))
  for(i in 1:length(unlist(task.clean))) {
    run[[i]] = getOMLRun(unlist(task.clean)[i])
    expect_is(run[[i]], "OMLRun")
  }
})
