context("checkUserConfirmation")

test_that("checkUserConfirmation", {
  reset_config({
    setOMLConfig(confirm.upload = FALSE)
    expect_true(checkUserConfirmation("flow"))
  })
})
