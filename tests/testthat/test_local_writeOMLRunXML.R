test_that("writeOMLRunXML", {
  with_test_cache({
    run = getOMLRun(1)
    xml.file = tempfile()
    run.xml = writeOMLRunXML(run, xml.file)
    expect_true(file.exists(xml.file))

    doc = readLines(xml.file, warn = FALSE)
    expect_true(length(doc) > 0)

    run$parameter.setting[[1]]$value = function(x) x
    expect_error(writeOMLRunXML(run, xml.file), "currently not supported")
  })
})
