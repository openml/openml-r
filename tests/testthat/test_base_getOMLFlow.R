context("getOMLFlow")

test_that("getOMLFlow", {
  flow = getOMLFlow(2L)
  expect_is(flow, "OMLFlow")
  expect_true(flow$implementation.id == 2L)

  expect_error(getOMLFlow(implementation.id = -1L))
})
