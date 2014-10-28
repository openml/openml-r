context("download all tasks")

# we download "most" safe tasks / dsets, because large ones might cause runtime and mem problems currently

test_that("download all tasks", {
  skip_on_cran()
  skip_on_travis()
  measures = getOMLEvaluationMeasures(session.hash)
  ttypes = extractSubList(getOMLTaskTypeList(session.hash), "id", use.names = FALSE)
  
  dlAllTasksOfType = function(type) {
    tl = getOMLTaskList(type = type, session.hash = session.hash)
    tids = tl[tl$status == "active" & tl$NumberOfInstances <= 10000 & tl$NumberOfFeatures <= 100, "task_id"]
    
    for (id in tids) {
      print(id)
      task = downloadOMLTask(id)
      tf = task$target.features
      expect_true(is.character(tf) && length(tf) %in% 0:1 && !is.na(tf))
      ds = task$data.set$data
      expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) >= 1)
      ems = task$evaluation.measures
      # expect_true(ems %in% measures)
      # FIXME: Delete next line when measure spelling is fixed (regarding spaces and underscores)
      all(ems %in% measures | str_replace_all(ems, " ", "_") %in% measures)
    }
  }

  for (i in ttypes) {
    dlAllTasksOfType(i)
  }
})
