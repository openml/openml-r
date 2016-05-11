context("convertOMLSplitsToMlr")

test_that("convertOMLSplitsToMlr", {
  with_test_cache({
    task = getOMLTask(59)
    mlr.task = convertOMLTaskToMlr(task)$mlr.task
    
    splits = convertOMLSplitsToMlr(task$input$estimation.procedure, mlr.task)
    expect_is(splits, "ResampleInstance")
    expect_equal(names(splits), c("desc", "size", "train.inds", "test.inds", "group"))
    expect_equal(splits$size, getTaskSize(mlr.task))
  })
})
