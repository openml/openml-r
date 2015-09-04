context("getOMLFlow")

test_that("getOMLFlow", {
  flow = getOMLFlow(2L)
  expect_is(flow, "OMLFlow")
  expect_true(flow$flow.id == 2L)

  expect_error(getOMLFlow(flow.id = -1L))
})
