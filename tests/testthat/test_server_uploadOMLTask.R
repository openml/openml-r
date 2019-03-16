context("uploadOMLTask")

test_that("uploadOMLTask throws an error if task already exists", {
  with_test_cache({
    # test on gina dataset (id = 41158)
    ds = getOMLDataSet(41158L)
    gettask = function() { uploadOMLTask(1L, ds$desc$id, ds$target.features, 1L, verbosity = 1) }
    expect_error(gettask())
  })
})

test_that("uploadOMLTask returns an task.id if task successfully created", {
  with_test_cache({
    # test on gina dataset (id = 41158)
    ds = getOMLDataSet(41158L)
    task.id = uploadOMLTask(1L, ds$desc$id, ds$target.features, 2L)
    expect_is(task.id, "integer")
    deleteOMLObject(task.id, object = "task")
  })
})
