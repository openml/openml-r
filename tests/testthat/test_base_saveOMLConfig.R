context("saveOMLConfig")

test_that("saveOMLConfig", {
  # load config file
  curr.conf = as.list(getOMLConfig()) #as.list(loadOMLConfig())

  # Overwrite config file only if overwrite = TRUE is expicitly set
  expect_error(saveOMLConfig(arff.reader = "RWeka"), "Set override to TRUE to force overwriting.")

  # change arff.reader and check if changes were stored in config file
  saveOMLConfig(arff.reader = "RWeka", overwrite = TRUE)
  conf.RWeka = as.list(loadOMLConfig())
  expect_true(conf.RWeka$arff.reader == "RWeka")
  expect_equal(conf.RWeka, as.list(getOMLConfig()))
  saveOMLConfig(arff.reader = "farff", overwrite = TRUE)
  conf.farff = as.list(loadOMLConfig())
  expect_true(conf.farff$arff.reader == "farff")
  expect_equal(conf.farff, as.list(getOMLConfig()))

  # set previous configs
  do.call("setOMLConfig", curr.conf)
})
