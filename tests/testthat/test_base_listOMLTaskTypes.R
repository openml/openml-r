context("listOMLTaskTypes")

test_that("listOMLDataSets", {
  tt = listOMLTaskTypes()
  expect_is(tt, "data.frame")
  expect_true(nrow(tt) > 1L && ncol(tt) == 2L)
  expect_true(setequal(names(tt), c("id", "name")))
})
