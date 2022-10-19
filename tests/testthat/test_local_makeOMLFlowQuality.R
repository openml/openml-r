test_that("makeOMLFlowQuality", {
  fq = makeOMLFlowQuality("Quality", "value")
  expect_s3_class(fq, "OMLFlowQuality")
  #expect_output(print(fq), "Quality")
  expect_character(print(fq), pattern = "Quality")
})
