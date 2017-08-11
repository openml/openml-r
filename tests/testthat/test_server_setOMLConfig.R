context("setOMLConfig")

test_that("setOMLConfig", {
  with_test_server({
    with_reset_config({
      conf = getOMLConfig()
      expect_is(conf, "OMLConfig")
      checkConfig(conf)
      conf.list = as.list(conf)
      conf.list$verbosity = 0L

      conf.set = do.call("setOMLConfig", conf.list)
      expect_true(getOMLConfig()$verbosity == 0L)
      expect_identical(as.list(getOMLConfig()), conf.list)
      conf.set = as.list(conf.set)

      expect_error(setOMLConfig(verbosity = 3L))
      expect_error(setOMLConfig(apikey = "toshort"), "The apikey must contain 32 characters")
      conf.set2 = as.list(setOMLConfig(verbosity = 2L))

      # verbosity not equal
      expect_true(conf.set$verbosity != conf.set2$verbosity)
      # everything else is equal
      expect_identical(conf.set[!grepl("verbosity", names(conf.set))],
        conf.set2[!grepl("verbosity", names(conf.set2))])

      with_empty_cache({
        key = getOMLConfig()$apikey
        setOMLConfig(apikey = paste(rep("a", 32), collapse = ""))
        expect_error(getOMLDataSet(1), "Authentication failed") # should fail

        setOMLConfig(apikey = key)
        ds = getOMLDataSet(1) # should work again
        expect_is(ds, "OMLDataSet")
        expect_true(ds$desc$id == 1L)
      })
    })
  })
})
