context("getOMLDataQualityList")

test_that("getOMLDataQualityList", {
  dqn = getOMLDataQualityList(session.hash)
  expect_true(is.character(dqn) && length(dqn) > 50L)
})