context("createOpenMLImplementationForMLRLearner")

test_that("createOpenMLImplementationForMLRLearner", {
  lrn <- makeLearner("classif.J48")
  oml.impl <- createOpenMLImplementationForMLRLearner(lrn, description = "blabla")
  expect_is(oml.impl, "OpenMLImplementation")
  expect_equal(oml.impl@description, "blabla")

  expect_error(createOpenMLImplementationForMLRLearner(lrn = "lrn"))
})
