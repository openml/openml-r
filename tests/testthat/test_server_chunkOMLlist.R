context("chunkOMLlist")

test_that("chunkOMLlist", {
  expect_chunk = function(listfun, ..., total.limit = 100, chunk.limit = 50) {
    d1 = do.call(listfun, args = c(list(...), list(limit = total.limit)))
    d2 = chunkOMLlist(listfun, ..., total.limit = total.limit, chunk.limit = chunk.limit)
    expect_equal(d1, d2)
  }

  # check task
  expect_chunk("listOMLTasks")

  # check datasets
  expect_chunk("listOMLDataSets")

  # check flows
  expect_chunk("listOMLFlows")
  expect_chunk("listOMLFlows", total.limit = 2, chunk.limit = 1)
  expect_chunk("listOMLFlows", total.limit = 100000, chunk.limit = 10000)

  # check runs
  expect_chunk("listOMLRuns", task.id = 59)
  expect_chunk("listOMLRuns", task.id = 59, total.limit = 2, chunk.limit = 1)

  # check run evaluations
  expect_chunk("listOMLRunEvaluations", evaluation.measure = "predictive_accuracy", task.id = 59)
  expect_chunk("listOMLRunEvaluations", evaluation.measure = "predictive_accuracy", task.id = 59, total.limit = 2, chunk.limit = 1)
})
