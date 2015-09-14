context("getOMLRun")

test_that("getOMLRun", {
  clearOMLCache()
  expect_error(getOMLRun(1L, cache.only = TRUE), "not found in cache")

  run = getOMLRun(1L)
  expect_is(run, "OMLRun")
  expect_true(run$run.id == 1L)

  expect_error(getOMLRun(run.id = -1L))
  expect_error(getOMLRun(run.id = 1464351321L), "Run not found")

  expect_is(run$predictions, "data.frame")
  
  # try different runs of different task types
  #tasks = listOMLTasks()
  tasks = tasks[with(tasks, NumberOfInstances < 1000 & NumberOfFeatures < 1000 &
                       (NumberOfSymbolicFeatures==0 | is.na(NumberOfSymbolicFeatures))), ]
  task.ids = split(tasks$task_id, tasks$task_type)
  task.ids = lapply(task.ids, function(X) tail(X, 3))
  
  task.clean = lapply(unlist(task.ids), function(X) {
    r = try(listOMLRuns(task.id = X), silent = TRUE)
    if (!is.error(r)) return(r) else return(NULL)
  } )
  task.clean = sapply(filterNull(task.clean), function(X) tail(X$run.id, 3))
  
  run = vector("list", length(unlist(task.clean)))
  for(i in 1:length(unlist(task.clean))) {
    run[[i]] = getOMLRun(unlist(task.clean)[i])
    expect_is(run[[i]], "OMLRun")
  }
})
