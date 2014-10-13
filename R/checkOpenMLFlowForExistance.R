checkOpenMLFlowForExistance = function(flow, show.info = getOpenMLOption("show.info")) {
  assertClass(flow, "OpenMLImplementation")
  assertFlag(show.info)
  fn.impl.xml = tempfile()
  on.exit({
    unlink(fn.impl.xml)
  })
  downloadAPICallFile(api.fun = "openml.implementation.exists", file = fn.impl.xml, name = flow$name,
    external_version = flow$external.version, show.info = show.info)
  doc = parseXMLResponse(fn.impl.xml, "Checking existance of flow", "implementation_exists")
  exists = as.logical(xmlRValS(doc, "/oml:implementation_exists/oml:exists"))
  id = xmlOValI(doc, "/oml:implementation_exists/oml:id")
  return(list(exists = exists, id = id))
}