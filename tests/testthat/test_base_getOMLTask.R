context("getOMLTask")

test_that("getOMLTask", {
  measures = listOMLEvaluationMeasures(session.hash)$name

  task = getOMLTask(1L, session.hash)
  expect_is(task, "OMLTask")
  expect_is(task$data.set, "OMLDataSet")
  expect_true(is.null(task$data.set$data))

  tf = task$target.features
  expect_true(is.character(tf) && length(tf) %in% 0:1 && !is.na(tf))

  ems = task$evaluation.measures
  expect_true(all(ems %in% measures | str_replace_all(ems, " ", "_") %in% measures))

  expect_is(task$preds, "list")

  expect_error(getOMLTask(1231109283L, session.hash))
})
