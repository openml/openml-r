context("listOMLTasks")

test_that("listOMLTasks", {
  tasks.cl = listOMLTasks(1L, session.hash)
  expect_is(tasks.cl, "data.frame")
  expect_true(nrow(tasks.cl) > 5L)
  expect_true(all(tasks.cl$task_type == "Supervised Classification"))

  tasks.reg = listOMLTasks(2L, session.hash)
  expect_is(tasks.reg, "data.frame")
  expect_true(nrow(tasks.cl) > 1L)
  expect_true(all(tasks.reg$task_type == "Supervised Regression"))

  expect_true(length(intersect(tasks.cl$task_id, tasks.reg$task_id)) == 0)

  expect_error(listOMLTasks(0L, session.hash))
})
