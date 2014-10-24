context("checkOpenMLFlowForExistence")

test_that("checkOpenMLFlowForExistence", {
  lrn = makeLearner("classif.rpart")
  flow = createOpenMLImplementationForMlrLearner(lrn)
  check = checkOpenMLFlowForExistence(flow)
  expect_is(check, "list")
  expect_true(check$exists)
  expect_is(check$id, "integer")
  
  flow$name = "imaginary_non-existent_flow"
  check = checkOpenMLFlowForExistence(flow)
  expect_is(check, "list")
  expect_false(check$exists)
  expect_is(check$id, "integer")
})
