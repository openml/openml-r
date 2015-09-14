context("getOMLTask")

test_that("getOMLTask", {
  clearOMLCache()
  expect_error(getOMLTask(1L, cache.only = TRUE), "not found in cache")

  measures = listOMLEvaluationMeasures()$name

  task = getOMLTask(1L)
  expect_is(task, "OMLTask")
  expect_is(task$input$data.set, "OMLDataSet")
  expect_true(is.data.frame(task$input$data.set$data))

  tf = task$input$data.set$target.features
  expect_true(is.character(tf) && length(tf) %in% 0:1 && !is.na(tf))

  ems = task$input$evaluation.measures
  expect_true(all(ems %in% measures | stri_replace_all_fixed(ems, " ", "_") %in% measures))

  expect_is(task$output$predictions, "list")

  expect_error(getOMLTask(1231109283L),  "Unknown task")
  
  # try different tasks of different task types
  #tasks = listOMLTasks()
  tasks = tasks[with(tasks, NumberOfInstances < 1000 & NumberOfFeatures < 1000 &
                       (NumberOfSymbolicFeatures==0 | is.na(NumberOfSymbolicFeatures))), ]
  task.ids = split(tasks$task_id, tasks$task_type)
  task.ids = lapply(task.ids, function(X) tail(X, 3))
  
  for(i in unlist(task.ids)) expect_is(getOMLTask(i), "OMLTask")
})
