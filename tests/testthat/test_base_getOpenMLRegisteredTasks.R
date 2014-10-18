context("getOpenMLRegisteredTasks")

test_that("getOpenMLRegisteredTasks", {
  rt = getOpenMLRegisteredTasks()
  expect_is(rt, "data.frame")
  expect_equal(colnames(rt), c("task_id", "data_name", "data_version"))
  expect_true(nrow(rt) > 1L)
})
