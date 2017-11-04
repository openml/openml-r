checkOMLDataSet = function(data) {
  expect_is(data, "OMLDataSet")
  expect_output(print(data), "Data Set")
  expect_is(data$desc, "OMLDataSetDescription")
  expect_character(data$colnames.old, any.missing = FALSE)
  expect_character(data$colnames.new, any.missing = FALSE)
}

checkBMR = function(bmr) {
  expect_is(bmr, "BenchmarkResult")
  res.classes = c("character", "character", "TaskDesc", "data.frame", "data.frame", "numeric",
    "Prediction", "list", "data.frame", "list", "numeric", "Learner")
  for (j in seq_along(res.classes)) expect_is(bmr$results[[1]][[1]][[j]], res.classes[j])
  expect_equal(dim(bmr$results[[1]][[1]]$measures.train), dim(bmr$results[[1]][[1]]$measures.test))
  for (j in seq_along(bmr$measures)) {
    expect_is(bmr$measures[[j]], "Measure")
    expect_equal(getBMRMeasures(bmr)[[j]], bmr$measures[[j]])
    expect_equal(getBMRMeasureIds(bmr)[[j]],  bmr$measures[[j]]$id)
  }
  for (j in seq_along(bmr$learners)) expect_is(bmr$learners[[j]], "Learner")

  # check getBMRPredictions
  preds = getBMRPredictions(bmr, as.df = FALSE)
  expect_true(is.list(preds))
  preds1 = preds[[1L]]
  expect_true(is.list(preds1))
  preds11 = preds1[[1L]]
  expect_is(preds11, "Prediction")

  p = getBMRPerformances(bmr, as.df = TRUE)
  expect_is(p, "data.frame")
  expect_true(nrow(p) > 1)

  a = getBMRAggrPerformances(bmr, as.df = TRUE)
  expect_is(a, "data.frame")
  expect_true(nrow(a) == 1)
}
