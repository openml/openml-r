context("writeOMLDataSetXML")

test_that("writeOMLDataSetXML", {
  with_test_cache({
    data = getOMLDataSet(61)
    xml.file = tempfile()
    data.xml = writeOMLDataSetXML(data$desc, xml.file)
    expect_true(file.exists(xml.file))

    # compare with local xml
    doc = xmlParse(xml.file)
    prev = as.list(getOMLConfig())
    doc2 = xmlParse(normalizePath(paste0(prev$cachedir, "/datasets/61/description.xml")))
    expect_equal(doc, doc2)
  })
})
