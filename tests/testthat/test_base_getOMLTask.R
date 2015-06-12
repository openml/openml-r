context("getOMLTask")

test_that("getOMLTask", {
  measures = listOMLEvaluationMeasures(session.hash)$name

  task = getOMLTask(1L, session.hash)
  expect_is(task, "OMLTask")
  expect_is(task$input$data.set, "OMLDataSet")
  expect_true(is.data.frame(task$input$data.set$data))

  tf = task$input$data.set$target.features
  expect_true(is.character(tf) && length(tf) %in% 0:1 && !is.na(tf))

  ems = task$input$evaluation.measures
  expect_true(all(ems %in% measures | stri_replace_all_fixed(ems, " ", "_") %in% measures))

  expect_is(task$output$predictions, "list")

  expect_error(getOMLTask(1231109283L, session.hash),  "Unknown task")
})
