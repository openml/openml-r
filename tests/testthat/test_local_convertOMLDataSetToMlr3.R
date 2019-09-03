context("convertOMLDataSetToMlr3")

test_that("convertOMLDataSetToMlr3", {
  with_test_cache({
    ds = getOMLDataSet(10)

    expect_is_mlr_task = function(mlr.task, ds) {
      expect_equal(mlr.task$task_type, "classif")
      expect_equal(mlr.task$nrow, nrow(ds$data))
      expect_equal(ds$desc$default.target.attribute, mlr.task$target_names)
    }

    # now create the task
    mlr.task = convertOMLDataSetToMlr3(ds)
    expect_equal(mlr.task$task_type, "classif")

    # now modify dataset by hand (no more server calls) to check
    # ignore attributes stuff:
    # Define the first two attributes as ignored attributes
    ds$desc$ignore.attribute = colnames(ds$data[, 1:2])

    mlr.task = convertOMLDataSetToMlr3(ds, ignore.flagged.attributes = TRUE)
    expect_is_mlr_task(mlr.task, ds)
    # we removed two attributes (and the target column is not considered here)
    #expect_equal(sum(mlr.task$task.desc$n.feat), ncol(ds$data) - 3L)
    expect_equal(mlr.task$ncol, ncol(ds$data) - 2L)

    # pass faulty parameters
    expect_error(convertOMLDataSetToMlr3(ds, task.type = "Nonexistent task type"), "element of")

    # check setting mlr task id
    expect_equal(convertOMLDataSetToMlr3(ds)$id, ds$desc$name)
    expect_equal(convertOMLDataSetToMlr3(ds, mlr.task.id = "<oml.data.name>.<oml.data.id>")$id,
      sprintf("%s.%s", ds$desc$name, ds$desc$id))
    expect_equal(convertOMLDataSetToMlr3(ds, mlr.task.id = "test")$id, "test")
    expect_equal(convertOMLDataSetToMlr3(ds, mlr.task.id = "<oml.data.id>")$id, as.character(ds$desc$id))
    expect_equal(convertOMLDataSetToMlr3(ds, mlr.task.id = "<oml.data.name>")$id, as.character(ds$desc$name))
    expect_equal(convertOMLDataSetToMlr3(ds, mlr.task.id = "<oml.data.version>")$id, as.character(ds$desc$version))
    expect_equal(convertOMLDataSetToMlr3(ds, mlr.task.id = "<oml.task.id>")$id, "<oml.task.id>")

    # check if conversion to regression task works
    ds$desc$target.features = ds$desc$default.target.attribute = "no_of_nodes_in"
    expect_equal(convertOMLDataSetToMlr3(ds)$task_type, "regr")
  })
})
