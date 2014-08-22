context("download all tasks")

test_that("download all tasks", {
  # FIXME: extend test to all task on openml server
  # FIXME: problem in task 6, 8
  ids = 1:10
  for (i in ids) {
    print(i)
    task = downloadOpenMLTask(id = i, show.info = TRUE, clean.up = TRUE)
    tf = task$target.features
    expect_true(is.character(tf) && length(tf) == 1 && !is.na(tf))
    ds = task$data.desc$data.set
    expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) > 1 )
    if (task$task.type == "Supervised Classification")
      lrn = makeLearner("classif.rpart")
    else if (task$task.type == "Supervised Regression")
      lrn = makeLearner("regr.rpart")
    res = runTask(task, lrn)
  }
})
