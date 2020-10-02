writeOMLStudyXML = function(description, file) {
  assertClass(description, "OMLStudy")
  assertPathForOutput(file, overwrite = TRUE)

  doc = newXMLDoc()
  top = newXMLNode("oml:study", parent = doc, namespace = c(oml = "http://openml.org/openml"))

  mynode = function(name, val, parent = top, attrs = NULL){
    if (!is.na(val))
      newXMLNode(name, as.character(val), parent = parent, namespace = "oml", attrs = attrs)
  }
  mynode(name = "alias", val = description$alias, parent = top)
  mynode(name = "main_entity_type", val = description$main.entity.type, parent = top)
  mynode(name = "name", val = description$name, parent = top)
  mynode(name = "description", val = description$description, parent = top)

  elements = list(
    data = description$data$data.id,
    tasks = description$tasks$task.id,
    flows = description$flows$flow.id,
    runs = description$runs$run.id
  )
  elements = filterNull(elements)

  for (el in names(elements)) {
    element = newXMLNode(el, parent = top, namespace = "oml")
    id.name = paste0(gsub("s$", "", el), "_id")
    lapply(elements[[el]], function(i) mynode(id.name, i, parent = element))
  }

  saveXML(top, file = file)
}
