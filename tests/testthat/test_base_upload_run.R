context("upload run")

test_that("upload run", {
  hash = authenticateUser("openml.rteam@gmail.com", "testpassword")
  
  lrn = makeLearner("classif.JRip")
  flow = createOpenMLImplementationForMLRLearner(lrn)
  sourcefile = generateSourcefileForMlrLearner(flow)
  flow.id = uploadOpenMLImplementation(flow, sourcefile = sourcefile, session.hash = hash, 
    delete.source.binary = TRUE)
  expect_is(flow.id, "integer")
  
  task = downloadOpenMLTask(4)
  res = runTask(task, lrn) 
  
  run.id = uploadOpenMLRun(task, lrn, flow.id, res, session.hash = hash)
  expect_is(run.id, "integer")
  
  lrn$par.vals = list(F = 2, N = 1)
  res = runTask(task, lrn)
  
  run.id2 = uploadOpenMLRun(task, lrn, flow.id, res, session.hash = hash)
  expect_is(run.id, "integer")
  rr = downloadOpenMLRunResults(run.id2)
  # expect_true(all(extractSubList(rr$parameter.setting, element = "name") == c("F", "N")))
  expect_true(all(extractSubList(rr$parameter.setting, element = "value") == c("2", "1")))
})
