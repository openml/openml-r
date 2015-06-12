context("download all tasks")

# we download "most" safe tasks / dsets, because large ones might cause runtime and mem problems currently

test_that("download all tasks", {
  skip_on_cran()
  skip_on_travis()
  measures = listOMLEvaluationMeasures(session.hash)$name
  ttypes = listOMLTaskTypes(session.hash)$id

  dlAllTasksOfType = function(type) {
    tl = listOMLTasks(type = type, session.hash = session.hash)
    tids = tl[tl$status == "active" & tl$NumberOfInstances <= 10000 & tl$NumberOfFeatures <= 100, "task_id"]

    for (id in tids) {
      print(id)
      task = getOMLTask(id, session.hash)
      tf = task$input$data.set$target.features
      expect_true(is.character(tf) && length(tf) %in% 0:1 && !is.na(tf))
      ds = task$input$data.set$data
      expect_true(is.data.frame(ds) && nrow(ds) > 1  && ncol(ds) >= 1)
      ems = task$input$evaluation.measures
      # expect_true(ems %in% measures)
      # FIXME: Delete next line when measure spelling is fixed (regarding spaces and underscores)
      expect_true(all(ems %in% measures | stri_replace_all_fixed(ems, " ", "_") %in% measures))
    }
  }

  for (i in ttypes) {
    dlAllTasksOfType(i)
  }
})
