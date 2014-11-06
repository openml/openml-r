context("getOMLDataSetList")

test_that("getOMLDataSetList", {
  dsl = getOMLDataSetList(session.hash)
  expect_is(dsl, "data.frame")
  expect_true(nrow(dsl) > 1L && ncol(dsl) == 8L)
})
