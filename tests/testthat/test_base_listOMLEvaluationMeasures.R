context("listOMLEvaluationMeasures")

test_that("listOMLEvaluationMeasures", {
  df = listOMLEvaluationMeasures(session.hash)
  expect_true(is.data.frame(df))
  expect_true(nrow(df) > 30)
  expect_true(ncol(df) == 1)
  expect_true(setequal(names(df), "name"))
})
