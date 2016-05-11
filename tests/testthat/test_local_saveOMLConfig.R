context("saveOMLConfig")

test_that("saveOMLConfig", {
  reset_config({
    # change arff.reader and check if changes were stored in config file
    saveOMLConfig(arff.reader = "RWeka", overwrite = TRUE)
    conf.RWeka = as.list(loadOMLConfig())
    expect_true(conf.RWeka$arff.reader == "RWeka")
    
    expect_error(saveOMLConfig(arff.reader = "RWeka",  overwrite = FALSE), "Set overwrite to TRUE to force overwriting.")
    
    saveOMLConfig(arff.reader = "farff", overwrite = TRUE)
    conf.farff = as.list(loadOMLConfig())
    expect_true(conf.farff$arff.reader == "farff")
  })
})
