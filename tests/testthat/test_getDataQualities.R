context("getDataQualities")

test_that("getDataQualities", {
  expect_is({dqn = getDataQualityNames()}, "character")
  expect_is(dq = getDataQualities(), "data.frame")
  expect_equal(cols = ncol(getDataQualities()), 10)
  expect_equal(cols = ncol(getDataQualities(set = "all")), length(getDataQualityNames()) + 1)
})