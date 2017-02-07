context("listOMLEstimationProcedures")

skip_on_cran()

test_that("listOMLEstimationProcedures", {
  df = .listOMLEstimationProcedures()
  expect_data_frame(df, min.rows = 5, ncols = 3)
  expect_set_equal(names(df), c("est.id", "task.type", "name"))
  expect_true("10-fold Crossvalidation" %in% df$name)
  expect_true("Supervised Classification" %in% df$task.type)
})
