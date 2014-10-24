context("getOMLEvaluationMeasures")

test_that("getOMLEvaluationMeasures", {
  em = getOMLEvaluationMeasures(session.hash)
  expect_is(em, "character")
  expect_true(length(em) > 50L)
})
