context("downloadOMLTask")

test_that("downloadOMLTask", {
  measures = getOMLEvaluationMeasures(session.hash)
  
  task = downloadOMLTask(1, session.hash)
  tf = task$target.features
  expect_true(is.character(tf) && length(tf) %in% 0:1 && !is.na(tf))
  ds = task$data.set$data
  expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) >= 1)
  ems = task$evaluation.measures
  # expect_true(ems %in% measures)
  # FIXME: Delete next line when measure spelling is fixed (regarding spaces and underscores)
  expect_true(all(ems %in% measures | str_replace_all(ems, " ", "_") %in% measures))
  expect_true(task$id == 1)
  expect_true(is.list(task$preds))
})