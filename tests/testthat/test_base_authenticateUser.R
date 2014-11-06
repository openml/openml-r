context("authenticateUser")

test_that("authenticateUser", {
  hash = authenticateUser("openml.rteam@gmail.com", "testpassword")
  expect_true(testString(hash))
  
  expect_error(authenticateUser("xxx", "xxx"))
})