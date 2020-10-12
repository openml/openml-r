context("convertOMLSplitsToMlr3")

test_that("convertOMLSplitsToMlr3", {
  with_test_cache({
    task = getOMLTask(59)
    mlr.task = convertOMLTaskToMlr3(task)$mlr.task

    oml.types = c("crossvalidation", "holdout")
    mlr.types = c("cv", "holdout")

    for (i in seq_along(oml.types)) {
      task$input$estimation.procedure$type = oml.types[i]
      if (oml.types[i] == "holdout") {
        task$input$estimation.procedure$parameters$percentage = "50"
      }
      splits = convertOMLSplitsToMlr3(task$input$estimation.procedure, mlr.task)
      expect_is(splits, "Resampling")
      expect_equal(splits$id, mlr.types[i])
    }

    # pass invalid estim.proc
    task$input$estimation.procedure$type = "blabla"
    expect_error(convertOMLSplitsToMlr3(task$input$estimation.procedure, mlr.task), "Unsupported estimation procedure type: blabla")
  })
})
