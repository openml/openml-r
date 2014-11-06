context("getOMLTaskTypeList")

test_that("getOMLTaskTypeList", {
  tt = getOMLTaskTypeList(session.hash)
  expect_true(is.list(tt) && length(tt) > 3L)
  expect_true("Supervised Classification" %in% extractSubList(tt, "name", simplify = TRUE))
})
