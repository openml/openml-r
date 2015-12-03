context("makeOMLFlowQuality")

test_that("makeOMLFlowQuality", {
  fq = makeOMLFlowQuality("Quality", "value")
  expect_is(fq, "OMLFlowQuality")
  expect_output(print(fq), "Quality")
})
