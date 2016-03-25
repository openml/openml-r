context("listOMLEvaluationMeasures")

skip_on_cran()

test_that("listOMLEvaluationMeasures", {
  df = .listOMLEvaluationMeasures()
  expect_data_frame(df, min.rows = 30, ncols = 1)
  expect_set_equal(names(df), "name")
})
