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
    conf = readConfigFile(conf.file)
    unlink(conf.file)
    return(conf)
  }

  # write a valid basic config file with username and password
  conf = writeAndReadConfig(username = "testuser", password = "1234", verbosity = 2)
  expect_identical(class(conf), class(getOMLConfig()))
  expect_equal(conf$username, "testuser")
  expect_null(conf$password)
  expect_true(!is.null(conf$pwdmd5))
  expect_true(is.integer(conf$verbosity))
  expect_output(print(conf), "configuration")

  # pass some invalid 'keys'
  expect_error(writeAndReadConfig(invalidProperty = "invalid"), "only allowed")
  expect_error(writeAndReadConfig(usernamee = "testuser"))
})
