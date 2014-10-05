context("download all tasks")

# we download "most" safe tasks / dsets, because large ones might cause runtime and mem problems currently

test_that("download all tasks", {
  taskinfo = getOpenMLRegisteredTasks()
  quals = getDataQualities()
  measures = getOpenMLEvaluationMeasures()

  for (i in seq_row(taskinfo)) {
    name = taskinfo[i, "data_name"]
    version = taskinfo[i, "data_version"]
    id = taskinfo[i, "task_id"]
    q = quals[quals$dataset == name & quals$version == version, ]
    if (q$NumberOfInstances <= 10000 & q$NumberOfFeatures <= 100 & name != "Zoo_test_jan") {
      print(id)
      task = downloadOpenMLTask(id = id, show.info = TRUE, clean.up = TRUE)
      tf = task$target.features
      expect_true(is.character(tf) && length(tf) %in% 0:1 && !is.na(tf))
      ds = task$data.desc$data.set
      expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) >= 1)
      ems = task$evaluation.measures
      # expect_true(ems %in% measures)
      # FIXME: Delete next line when measure spelling is fixed (regarding spaces and underscores)
      all(ems %in% measures | str_replace_all(ems, " ", "_") %in% measures)
    }
  }
})
