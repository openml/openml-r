checkOpenMLFlowForExistence = function(flow, show.info = getOpenMLOption("show.info")) {
  assertClass(flow, "OpenMLImplementation")
  assertFlag(show.info)
  url = getAPIURL("openml.implementation.exists", name = flow$name, external_version = flow$external.version)
  doc = parseXMLResponse(url, "Checking existence of flow", "implementation_exists")
  list(
    exists = as.logical(xmlRValS(doc, "/oml:implementation_exists/oml:exists")),
    id = xmlOValI(doc, "/oml:implementation_exists/oml:id")
  )
}
