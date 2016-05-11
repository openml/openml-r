context("uploadOMLRun")

test_that("uploadOMLRun", {
  # download a run and reupload it
  run = getOMLRun(1)
  expect_is(run, "OMLRun")
  expect_is(run$flow.id, "integer")
  expect_is(run$run.id, "integer")
  expect_is(run$task.id, "integer")
  
  # with_read_only({
  #   expect_error(uploadOMLRun(run), "This is a read-only account")
  # })
  
  with_write_access({
    maxrun = max(.listOMLRuns(task.id = run$task.id)$run.id)
    run.id = uploadOMLRun(run)
    expect_is(run.id, "integer")
    expect_true(maxrun < run.id)
    deleteOMLObject(run.id, object = "run")
    
    run$flow.id = NA
    expect_error(uploadOMLRun(run), "Please provide a")
    
    # upload self-created run
    lrn = makeLearner("classif.rpart")
    task = getOMLTask(59L)
    res = runTaskMlr(task, lrn, scimark.vector = rep(1.5, 6))
    run.id = uploadOMLRun(res)
    expect_is(run.id, "integer")
    deleteOMLObject(run.id, object = "run")
    
    # upload runTaskMlr Run
    run.id = uploadOMLRun(res)
    expect_is(run.id, "integer")
    deleteOMLObject(run.id, object = "run")
    
    # upload wrapped learner
    lrn = makeImputeWrapper(lrn, classes = list(numeric = imputeMedian(), integer = imputeMedian()))
    lrn = makeFilterWrapper(lrn, fw.perc = 0.5)
    
    res = runTaskMlr(task, lrn)
    run.id = uploadOMLRun(res)
    expect_is(run.id, "integer")
    deleteOMLObject(run.id, object = "run")
  })
  
})
