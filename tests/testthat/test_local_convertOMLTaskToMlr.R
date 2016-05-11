context("convertOMLTaskToMlr")

test_that("convertOMLTaskToMlr", {
  with_test_cache({
    task = getOMLTask(59)
    
    mlr.task = convertOMLTaskToMlr(task)
    expect_is(mlr.task$mlr.task, "Task")
    expect_is(mlr.task$mlr.rin, "ResampleInstance")
    for (i in seq_along(mlr.task$mlr.measures)) 
      expect_is(mlr.task$mlr.measures[[i]], "Measure")
  })
})
