checkOMLDataSet = function(data) {
  expect_s3_class(data, "OMLDataSet")
  expect_output(print(data), "Data Set")
  expect_s3_class(data$desc, "OMLDataSetDescription")
  expect_character(data$colnames.old, any.missing = FALSE)
  expect_character(data$colnames.new, any.missing = FALSE)
}

checkBMR = function(bmr) {
  expect_s3_class(bmr, "BenchmarkResult")
  res.classes = c("character", "character", "TaskDesc", "data.frame", "data.frame", "numeric",
    "Prediction", "list", "data.frame", "list", "numeric", "Learner")
  for (j in seq_along(res.classes)) expect_s3_class(bmr$results[[1]][[1]][[j]], res.classes[j])
  expect_equal(dim(bmr$results[[1]][[1]]$measures.train), dim(bmr$results[[1]][[1]]$measures.test))
  for (j in seq_along(bmr$measures)) {
    expect_s3_class(bmr$measures[[j]], "Measure")
    expect_equal(getBMRMeasures(bmr)[[j]], bmr$measures[[j]])
    expect_equal(getBMRMeasureIds(bmr)[[j]],  bmr$measures[[j]]$id)
  }
  for (j in seq_along(bmr$learners)) expect_s3_class(bmr$learners[[j]], "Learner")

  # check getBMRPredictions
  preds = getBMRPredictions(bmr, as.df = FALSE)
  expect_true(is.list(preds))
  preds1 = preds[[1L]]
  expect_true(is.list(preds1))
  preds11 = preds1[[1L]]
  expect_s3_class(preds11, "Prediction")

  p = getBMRPerformances(bmr, as.df = TRUE)
  expect_s3_class(p, "data.frame")
  expect_true(nrow(p) > 1)

  a = getBMRAggrPerformances(bmr, as.df = TRUE)
  expect_s3_class(a, "data.frame")
  expect_true(nrow(a) == 1)
}
