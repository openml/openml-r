context("uploadOMLTask")

test_that("uploadOMLTask", {
  with_test_cache({
    # test on gina dataset (id = 41158)
    ds = getOMLDataSet(41158L)
    task = uploadOMLTask(1L, ds$desc$id, ds$target.features, 1L)
    expect_equal(task, NA)
    
    task2 = uploadOMLTask(1L, ds$desc$id, ds$target.features, 2L)
    expect_is(task2, "integer")
    deleteOMLObject(task2, object = "task")
  })
})
