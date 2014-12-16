context("getOMLFlow")

test_that("getOMLFlow", {
  flow = getOMLFlow(1L, session.hash)
  expect_is(flow, "OMLflow")
  expect_true(flow$flow.id == 1L)

  expect_error(getOMLFlow(id = -1L))
})
