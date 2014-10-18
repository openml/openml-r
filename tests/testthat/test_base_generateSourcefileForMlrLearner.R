context("generateSourcefileForMlrLearner")

test_that("generateSourcefileForMlrLearner", {
  lrn = makeLearner("classif.JRip")
  sf = generateSourcefileForMlrLearner(lrn)
  source(sf)
  unlink(sf)
  res = sourcedFlow(1)
  expect_is(res, "data.frame")
  expect_equal(dim(res), c(898, 10))
  
  lrn = makeBaggingWrapper(lrn, bw.iters = 5)
  sf = generateSourcefileForMlrLearner(lrn)
  source(sf)
  unlink(sf)
  res = sourcedFlow(1)
  expect_is(res, "data.frame")
  expect_equal(dim(res), c(898, 10))
})
