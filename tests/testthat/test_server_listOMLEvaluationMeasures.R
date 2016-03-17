context(".listOMLEvaluationMeasures")

test_that(".listOMLEvaluationMeasures", {
  df = .listOMLEvaluationMeasures()
  expect_is(df, "data.frame")
  expect_true(nrow(df) > 30L)
  expect_true(ncol(df) == 1L)
  expect_set_equal(names(df), "name")
})
