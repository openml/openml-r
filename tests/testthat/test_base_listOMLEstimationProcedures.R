context(".listOMLEstimationProcedures")

test_that(".listOMLEstimationProcedures", {
  df = .listOMLEstimationProcedures()
  expect_is(df, "data.frame")
  expect_true(nrow(df) > 5L)
  expect_true(ncol(df) == 2L)
  expect_true(all(names(df) == c("est.id", "name")))
  expect_true(any(grepl("Crossvalidation", df$name)))
})
