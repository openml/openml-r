context("listOMLTasks")

test_that("listOMLTasks", {
  tasks = listOMLTasks()
  expect_is(tasks, "data.frame")
  expect_true(nrow(tasks.cl) > 5L)
})
