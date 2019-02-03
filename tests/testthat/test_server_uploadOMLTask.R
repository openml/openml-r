context("uploadOMLTask")

test_that("uploadOMLTask throws an error if task already exists", {
  with_test_cache({
    # test on gina dataset (id = 41158)
    ds = getOMLDataSet(41158L)
    gettask = function() { uploadOMLTask(1L, ds$desc$id, ds$target.features, 1L, verbosity = 1) }
    expect_error(gettask())
  })
})

test_that("uploadOMLTask returns an integer if task was created", {
  with_test_cache({
    # test on gina dataset (id = 41158)
    ds = getOMLDataSet(41158L)
    task_id = uploadOMLTask(1L, ds$desc$id, ds$target.features, 2L)
    expect_is(task_id, "integer")
    deleteOMLObject(task_id, object = "task")
  })
})