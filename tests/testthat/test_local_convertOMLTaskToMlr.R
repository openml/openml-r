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

  ds = task$input$data.set
  # check setting mlr task id
  expect_equal(getTaskId(convertOMLTaskToMlr(task)$mlr.task), ds$desc$name)
  expect_equal(getTaskId(convertOMLTaskToMlr(task, mlr.task.id = "<oml.data.name>.<oml.task.id>")$mlr.task),
    sprintf("%s.%s", ds$desc$name, task$task.id))
  expect_equal(getTaskId(convertOMLTaskToMlr(task, mlr.task.id = "test")$mlr.task), "test")
  expect_equal(getTaskId(convertOMLTaskToMlr(task, mlr.task.id = "<oml.data.id>")$mlr.task), as.character(ds$desc$id))
  expect_equal(getTaskId(convertOMLTaskToMlr(task, mlr.task.id = "<oml.data.name>")$mlr.task), as.character(ds$desc$name))
  expect_equal(getTaskId(convertOMLTaskToMlr(task, mlr.task.id = "<oml.data.version>")$mlr.task), as.character(ds$desc$version))
  expect_equal(getTaskId(convertOMLTaskToMlr(task, mlr.task.id = "<oml.task.id>")$mlr.task), as.character(task$task.id))

  with_main_server({
    task = getOMLTask(3)
    mlr.task = convertOMLTaskToMlr(task)$mlr.rin
    task.stratify = task$input$estimation.procedure$parameters$stratified_sampling
    task.stratify = ifelse(is.null(task.stratify), FALSE, task.stratify == "true")
    mlr.task.stratify = mlr.task$desc$stratify
    expect_equal(task.stratify, mlr.task.stratify)
  })

  # check if ignore.attribute works
  with_main_server({
    task = getOMLTask(3954)
    ignore = task$input$data.set$desc$ignore.attribute
    row = task$input$data.set$desc$row.id.attribute
    mlrTaskCols = colnames(getTaskData(convertOMLTaskToMlr(task)$mlr.task))
    expect_false(ignore %in% mlrTaskCols)
    expect_false(row %in% mlrTaskCols)
  })
})
