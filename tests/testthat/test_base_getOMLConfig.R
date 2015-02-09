context("getOMLConfig")

test_that("getOMLConfig", {
  conf = getOMLConfig()
  expect_is(conf, "OMLConfig")
  expect_identical(class(conf), class(getOMLConfig()))
})