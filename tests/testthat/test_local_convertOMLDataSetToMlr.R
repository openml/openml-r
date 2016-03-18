context("convertOMLDataSetToMlr")

test_that("convertOMLDataSetToMlr", {
  ds = getOMLDataSet(10)

  expect_is_mlr_task = function(mlr.task, ds) {
    expect_equal(mlr.task$type, "classif")
    expect_equal(mlr.task$task.desc$size, nrow(ds$data))
    expect_equal(ds$desc$default.target.attribute, mlr.task$task.desc$target)
  }

  # now create the task
  mlr.task = convertOMLDataSetToMlr(ds)

  # now modify dataset by hand (no more server calls) to check
  # ignore attributes stuff:
  # Define the first two attributes as ignored attributes
  ds$desc$ignore.attribute = colnames(ds$data[, 1:2])

  mlr.task = convertOMLDataSetToMlr(ds, ignore.flagged.attributes = TRUE)
  expect_is_mlr_task(mlr.task, ds)
  # we removed two attributes (and the target column is not considered here)
  expect_equal(sum(mlr.task$task.desc$n.feat), ncol(ds$data) - 3L)

  # pass faulty parameters
  expect_error(convertOMLDataSetToMlr(ds, task.type = "Nonexistent task type"), "element of")
})
