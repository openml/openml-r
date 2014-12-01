context("listOMLRunResults")

test_that("listOMLRunResults", {
  rr = listOMLRunResults(task.id = 1L, session.hash)
  expect_is(rr, "data.frame")
  expect_true(nrow(rr) > 100L && ncol(rr) >= 20L)
  expect_true(all(c("run.id", "setup.id", "implementation.id", "implementation", "task.id", "task.type.id", "estim.proc") %in% names(rr)))
})