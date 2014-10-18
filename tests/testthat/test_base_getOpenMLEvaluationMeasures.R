context("getOpenMLEvaluationMeasures")

test_that("getOpenMLEvaluationMeasures", {
  em = getOpenMLEvaluationMeasures()
  expect_is(em, "character")
  expect_true(length(em) > 50L)
})
