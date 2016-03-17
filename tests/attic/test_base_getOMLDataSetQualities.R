context("getOMLDataSetQualities")

test_that("getOMLDataSetQualities", {
  qualities_list = listOMLDataSetQualities()
  dids = tasks$did[tasks$task_id %in% unlist(task.ids)]
  
  for (id in dids) {
    qual = getOMLDataSetQualities(id)
    expect_is(qual, "data.frame")
    expect_true(all(qual$name %in% qualities_list$name))
    expect_true(is.character(qual[, 1L]))
    expect_true(is.numeric(qual[, 2L]))
    expect_equal(names(qual), c("name", "value"))
  }
})
