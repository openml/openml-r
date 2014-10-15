context("downloadOpenMLData")

test_that("downloadOpenMLData", {
  dsd = downloadOpenMLData("iris")
  expect_is(dsd, "OpenMLDataSetDescription")
  expect_true(dsd$version == 1)

  dsd = downloadOpenMLData("iris", version = 3)
  expect_is(dsd, "OpenMLDataSetDescription")
  expect_true(dsd$version == 3)
  
  expect_warning({dsd = downloadOpenMLData("iris", version = 2000)})
  expect_is(dsd, "OpenMLDataSetDescription")
  
  expect_error(downloadOpenMLData("xxx123"), "No data set")
})

