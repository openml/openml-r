test_that("checkUserConfirmation", {
  with_reset_config({
    setOMLConfig(confirm.upload = FALSE)
    expect_true(checkUserConfirmation("flow"))
  })
})
