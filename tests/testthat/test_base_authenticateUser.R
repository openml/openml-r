context("authenticateUser")

test_that("authenticateUser", {
  hash = authenticateUser("openml.rteam@gmail.com", "testpassword")
  expect_true(is.character(hash) && length(hash) == 1)
  
  expect_error(authenticateUser("xxx", "xxx"))
})