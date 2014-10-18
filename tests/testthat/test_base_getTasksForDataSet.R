context("getTasksForDataSet")

test_that("getTasksForDataSet", {
  tasks.1 = getTasksForDataSet(1)
  expect_is(tasks_did1, "integer")
  tasks.2 = getTasksForDataSet(2)
  tasks.1and2 = getTasksForDataSet(c(1, 2))
  expect_equal(length(tasks.1and2), length(tasks.1) + length(tasks.2))
})
