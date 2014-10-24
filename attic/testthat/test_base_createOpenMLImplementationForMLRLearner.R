context("createOpenMLImplementationForMlrLearner")

test_that("createOpenMLImplementationForMlrLearner", {
  lrn = makeLearner("classif.J48")
  oml.impl = createOpenMLImplementationForMlrLearner(lrn, description = "blabla")
  expect_is(oml.impl, "OpenMLImplementation")
  expect_equal(oml.impl$description, "blabla")

  expect_error(createOpenMLImplementationForMlrLearner(lrn = "lrn"))
})
