context("getOMLDataQualities")

test_that("getOMLDataQualities", {
  dqn = getOMLDataQualityNames()
  dq1 = getOMLDataQualities()
  dq2 = getOMLDataQualities(set = "all")
  expect_is(dqn, "character")
  expect_is(dq1, "data.frame")
  expect_is(dq2, "data.frame")
  expect_true(ncol(dq1) == 12 && nrow(dq1) > 1)
  expect_true(setequal(dqn, setdiff(colnames(dq2), c("did", "dataset", "version"))))
})
