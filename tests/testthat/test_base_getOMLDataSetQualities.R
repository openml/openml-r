context("getOMLDataSetQualities")

test_that("getOMLDataSetQualities", {
  for (id in 1:2) {
    qual = getOMLDataSetQualities(id)
    expect_is(qual, "data.frame")
    #FIXME: getOMLDataSetQualities(2) contains only 9 qualities (none of them is ClassCount, ClassEntropy or NoiseToSignalRatio)
    #expect_true(all(c("ClassCount", "ClassEntropy", "NoiseToSignalRatio") %in% qual$name))
    expect_true(is.character(qual[, 1L]))
    expect_true(is.numeric(qual[, 2L]))
    expect_equal(names(qual), c("name", "value"))
  }
})
