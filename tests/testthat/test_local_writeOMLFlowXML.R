context("writeOMLFlowXML")

test_that("writeOMLFlowXML", {
  with_test_cache({
    flow = getOMLFlow(3884)
    xml.file = tempfile()
    flow.xml = writeOMLFlowXML(flow, xml.file)
    expect_true(file.exists(xml.file))
    
    # compare with local xml
    doc = xmlParse(xml.file)
    prev = as.list(getOMLConfig())
    doc2 = xmlParse(normalizePath(paste0(prev$cachedir, "/flows/3884/flow.xml")))
    expect_equal(doc, doc2)
  })
})
