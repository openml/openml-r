context("getDataQualities")

test_that("getDataQualities", {
  dqn = getDataQualityNames()
  dq1 = getDataQualities()
  dq2 = getDataQualities(set = "all")
  expect_is(dqn, "character")
  expect_is(dq1, "data.frame")
  expect_is(dq2, "data.frame")
  expect_equal(ncol(dq2), length(dqn) + 1)
  expect_true(setequal(make.names(dqn), colnames(dq2)[-1]))
})
