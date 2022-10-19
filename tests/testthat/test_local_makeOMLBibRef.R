test_that("makeOMLBibRef", {
  br = makeOMLBibRef("OpenML et al.", "http://www.openml.org/")
  expect_output(print(br), "OpenML")
  expect_s3_class(br, "OMLBibRef")
})
