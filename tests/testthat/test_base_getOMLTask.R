context("getOMLTask")

test_that("getOMLTask", {
  clearOMLCache()
  expect_error(getOMLTask(1L, cache.only = TRUE), "not found in cache")
  
  measures = listOMLEvaluationMeasures()$name
  
  expect_error(getOMLTask(1231109283L),  "Unknown task")
  
  # try different tasks of different task types
  task.ids = split(tasks$task.id, tasks$task.type)
  task.ids = lapply(task.ids, function(X) head(X, 2))
  
  #setOMLConfig(arff.reader = "RWeka")
  expect_error(getOMLTask(261), "For input string")
  
  for (i in unlist(task.ids)) {
    # FIXME: change this after https://github.com/openml/OpenML/issues/240 is fixed
    if(i %in% task.ids[["Clustering"]]) {
      expect_error(getOMLTask(i), "Task not providing datasplits.")
    } else {
      task = getOMLTask(i)
      # there is some output
      expect_output(print(task), "OpenML Task")
      expect_is(task, "OMLTask")
      # check if there is dataset
      expect_is(task$input$data.set, "OMLDataSet")
      expect_true(is.data.frame(task$input$data.set$data))
      # can rownames be converted to numerics without introducing NAs?
      rns = rownames(task$input$data.set$data)
      expect_true(sum(is.na(as.numeric(rns))) == 0)
      # rownames should be zero-based
      expect_true(min(as.numeric(rns)) == 0)
      expect_true(max(as.numeric(rns)) == length(rns) - 1)
      
      # check target
      tf = task$input$data.set$target.features
      expect_true(is.character(tf) && length(tf) %in% 0:1)
      if (length(tf) != 0) expect_true(!is.na(tf))
      
      # check evaluation measures
      ems = task$input$evaluation.measures
      if (ems != "")
        expect_true(all(ems %in% measures | stri_replace_all_fixed(ems, " ", "_") %in% measures))
      
      # check prediction
      expect_is(task$output$predictions, "list")
      
      # check data splits
      ds = task$input$estimation.procedure$data.splits
      expect_is(ds, "data.frame")
      if (nrow(ds) > 0 & ncol(ds) > 0) {
        expect_true(min(ds$rowid) == 1)
        expect_true(max(ds$rowid) == nrow(task$input$data.set$data))
      }
      
      # FIXME: this test should be in a separate file called parseOMLDataSplits
      # check if we can avoid using the slower match function in parseOMLDataSplits
      url.dsplits = task$input$estimation.procedure$data.splits.url
      if (url.dsplits != "No URL") {
        f = findCachedTask(i)
        # FIXME: see https://github.com/openml/website/issues/25 when this is solved, we might change this line:
        data = tryCatch(suppressWarnings(arff.reader(f$datasplits.arff$path)), error = function(e) NULL)
        if (!is.null(data)) {
          ri = data$rowid
          rns = rownames(task$input$data.set$data)
          rowid = if (min(data$rowid) == 0) (data$rowid+1) else data$rowid
          expect_equal(rowid, match(ri, rns))
        }
      }
    }
  }
})
