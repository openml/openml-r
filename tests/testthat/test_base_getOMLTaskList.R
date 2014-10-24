context("getOMLTaskList")

test_that("getOMLTaskList", {
  tasks.cl = getOMLTaskList(1L, session.hash)
  expect_is(tasks.cl, "data.frame")
  expect_true(nrow(tasks.cl) > 1L)
  expect_true(all(task.cl$task_type == "Supervised Classification"))
  
  tasks.reg = getOMLTaskList(2L, session.hash)
  expect_is(tasks.reg, "data.frame")
  expect_true(nrow(tasks.cl) > 1L)
  expect_true(all(tasks.reg$task_type == "Supervised Regression"))
  
  expect_true(length(intersect(tasks.cl$task_id, tasks.reg$task_id)) == 0)
})
