context("getOMLConfig")

test_that("getOMLConfig", {
  conf = getOMLConfig()
  expect_is(conf, "OMLConfig")
  expect_identical(class(conf), class(getOMLConfig()))
})

test_that("reading and writing config files", {
  writeConfigFile = function(filename, ...) {
    args = list(...)
    lines = paste(names(args), args, sep = "=")
    con = file(filename, "w")
    on.exit(close(con))
    writeLines(lines, con = con)
  }

  writeAndReadConfig = function(...) {
    conf.file = tempfile("config")
    writeConfigFile(conf.file, ...)
    conf = loadOMLConfig(conf.file, assign = FALSE)
    unlink(conf.file)
    return(conf)
  }

  # write a valid basic config file
  conf = writeAndReadConfig(apikey = collapse(rep("x", 32), ""), verbosity = 2)
  expect_identical(class(conf), class(getOMLConfig()))
  expect_true(is.integer(conf$verbosity))
  expect_output(print(conf), "configuration")

  # pass some invalid 'keys'
  expect_error(writeAndReadConfig(invalidProperty = "invalid"), "only allowed")
  expect_error(writeAndReadConfig(xxx = "testuser"))
})
