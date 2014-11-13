context("listOMLRuns")

test_that("listOMLRuns", {
  rl = listOMLRuns(task.id = 1, session.hash = session.hash)
  expect_is(rl, "data.frame")
  expect_true(all(rl$task.id == 1))
  expect_true(setequal(names(rl), c("run.id", "task.id", "setup.id", "impl.id", "uploader")))

  rl = listOMLRuns(setup.id = 1, session.hash = session.hash)
  expect_true(all(rl$setup.id == 1))
  
  rl = listOMLRuns(impl.id = 56, session.hash = session.hash)
  expect_true(all(rl$impl.id == 56))
})
