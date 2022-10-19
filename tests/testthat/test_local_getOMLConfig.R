test_that("getOMLConfig", {
  conf = getOMLConfig()
  expect_s3_class(conf, "OMLConfig")
  expect_equal(conf, loadOMLConfig())
  expect_output(print(conf), "configuration")
  expect_string(printableConfig(conf))
})
