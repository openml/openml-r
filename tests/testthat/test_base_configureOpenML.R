context("configureOpenML")

test_that("getOptions and configureOpenML", {
  opt = getOpenMLOptions()
  expect_equal(length(opt), 3L)

  configureOpenML("show.info" = TRUE)
  expect_true(getOpenMLOption("show.info"))

  do.call(configureOpenML, opt)
  expect_equal(getOpenMLOptions(), opt)
})
