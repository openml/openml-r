context("listOMLDataSetQualities")

test_that("listOMLDataSetQualities", {
  for (id in 1:2) {
    qual = listOMLDataSetQualities(id, session.hash)
    expect_is(qual, "data.frame")
    expect_true(all(c("ClassCount", "ClassEntropy", "NoiseToSignalRatio") %in% qual$name))
    expect_true(is.character(qual[, 1L]))
    expect_true(is.numeric(qual[, 2L]))
    expect_equal(names(qual), c("name", "value"))
  }
})
