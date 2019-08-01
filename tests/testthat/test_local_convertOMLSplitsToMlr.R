context("convertOMLSplitsToMlr")

test_that("convertOMLSplitsToMlr", {
  with_test_cache({
    task = getOMLTask(59)
    mlr.task = convertOMLTaskToMlr(task)$mlr.task

    oml.types = c("crossvalidation", "holdout", "leaveoneout")
    mlr.types = c("cross-validation", "holdout", "LOO")

    for (i in seq_along(oml.types)) {
      # here we cheat a bit and change the resample procedure manually
      task$input$estimation.procedure$type = oml.types[i]
      if (oml.types[i] == "holdout") task$input$estimation.procedure$parameters$percentage = "50"

      splits = convertOMLSplitsToMlr(task$input$estimation.procedure, mlr.task)
      expect_is(splits, "ResampleInstance")
      expect_equal(names(splits), c("desc", "size", "train.inds", "test.inds", "group"))
      expect_equal(splits$size, getTaskSize(mlr.task))

      # check for correct mlr name
      expect_equal(splits$desc$id, mlr.types[i])
    }

    # pass invalid estim.proc
    task$input$estimation.procedure$type = "blabla"
    expect_error(convertOMLSplitsToMlr(task$input$estimation.procedure, mlr.task), "Unsupported estimation procedure type: blabla")
  })
})
