context("download all tasks")

# we download "most" safe tasks / dsets, because large ones might cause runtime and mem problems currently

test_that("download all tasks", {
  taskinfo = getOpenMLRegisteredTasks()
  quals = getDataQualities()
  measures = getOpenMLEvaluationMeasures()
  
  # remove large tasks
  ok = sapply(seq_row(taskinfo), function(i) {
    x = taskinfo[i,]
    qq = quals[x$data_name,]
    qq$NumberOfInstances <= 10000 & qq$NumberOfFeatures <= 100
  })
  taskinfo2 = taskinfo[ok,]

  for (i in taskinfo2$task_id) {
    print(i)
    task = downloadOpenMLTask(id = i, show.info = TRUE, clean.up = TRUE)
    tf = task$target.features
    expect_true(is.character(tf) && length(tf) == 1 && !is.na(tf))
    ds = task$data.desc$data.set
    expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) > 1)
    ems = task$evaluation.measures
    # expect_true(ems %in% measures)
    # FIXME: Delete next line when measure spelling is fixed (regarding spaces and underscores)
    all(ems %in% measures | str_replace_all(ems, " ", "_") %in% measures)

    if (task$type == "Supervised Classification")
      lrn = makeLearner("classif.J48")
    else if (task$type == "Supervised Regression")
      lrn = makeLearner("regr.rpart")
    res = runTask(task, lrn)
  }
})
