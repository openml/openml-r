context(".listOMLDataSetQualities/server")

test_that(".listOMLDataSetQualities", {
  dsqs = .listOMLDataSetQualities()
  expect_is(dsqs, "data.frame")
  expect_true(nrow(dsqs) >= 64L && ncol(dsqs) == 1L)
  expect_set_equal(names(dsqs), "name")
  expect_character(dsqs[, 1L], any.missing = FALSE, all.missing = FALSE)
})
