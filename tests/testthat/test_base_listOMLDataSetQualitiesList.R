context("listOMLDataSetQualitiesList")

test_that("listOMLDataSetQualitiesList", {
  qualities_list = listOMLDataSetQualitiesList()
  expect_is(qualities_list, "data.frame")
  expect_true(nrow(qualities_list) >= 64L && ncol(qualities_list) == 1L)
  expect_true(setequal(names(qualities_list), c("name")))
  expect_true(all(apply(qualities_list[1], 2, is.character)))
})
