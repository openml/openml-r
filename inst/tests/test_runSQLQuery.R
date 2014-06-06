context("runSQLQuery")

test_that("runSQLQuery", {
  res = runSQLQuery("SELECT * FROM task")
  expect_true(is.data.frame(res) && nrow(res) > 1  && ncol(res) == 2 && is.numeric(res$task_id))
})  