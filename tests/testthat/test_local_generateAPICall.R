context("generateAPICall")

test_that("generateAPICall produces valid call for large integer values", {
  # github issue #254 (large integer bounds)
  api.call = generateAPICall("www.test.com", number.of.instances = c(100, 100000000))
  expect_true(!grepl("100..1e+08", api.call), info = "Large integer converted to scientific notation.")

  # lower bound larger than upper bound
  filters = c("number.of.instances", "number.of.features", "number.of.classes", "number.of.missing.values")
  for (filter in filters) {
    args = list("www.test.com", c(1000, 10))
    names(args) = c("api.call", filter)
    expect_error(do.call(generateAPICall, args),
      info = sprintf("Lower bound greater than upper bound for filter '%s'.", filter))
  }
})
