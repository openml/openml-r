context("listOMLDataSetQualities")

test_that("listOMLDataSetQualities", {
  qualities.ds1 = listOMLDataSetQualities(1L, session.hash)
  expect_is(qualities.ds1, "data.frame")
  expect_true(all(dim(qualities.ds1) == c(9, 2)))
  expect_true(all(apply(qualities.ds1[1], 2, is.character)))
  expect_true(all(apply(qualities.ds1[2], 2, is.numeric)))
  expect_true(setequal(names(qualities.ds1), c("name", "value")))

  qualities.ds2 = listOMLDataSetQualities(2L, session.hash)
  expect_is(qualities.ds2, "data.frame")
  expect_true(all(dim(qualities.ds2) == c(64, 2)))
  expect_true(all(apply(qualities.ds2[1], 2, is.character)))
  expect_true(all(apply(qualities.ds2[2], 2, is.numeric)))
  expect_true(setequal(names(qualities.ds2), c("name", "value")))
})
