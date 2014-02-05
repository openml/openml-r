context("downloadOpenMLDataAsMlrTask")

test_that("downloadOpenMLDataAsMlrTask", {
  task = downloadOpenMLDataAsMlrTask("iris")
  expect_is(task, "ClassifTask")

  expect_error(downloadOpenMLDataAsMlrTask("xxx123"), "No data set")
})

