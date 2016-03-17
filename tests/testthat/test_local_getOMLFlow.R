context("getOMLFlow")

test_that("getOMLFlow", {
  with_test_cache({
    expect_error(getOMLFlow(430408, cache.only = TRUE), "not found in cache")
    flow = getOMLFlow(2)
    expect_output(print(flow), "Flow")
    expect_equal(flow$flow.id, 2)
 })
})
