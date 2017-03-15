# Generate an XML file for an OpenMLImplementation object.
#
# @param description [\code{\link{OpenMLImplementation}}]\cr
#   OpenML implementation description object.
# @param file [\code{character(1)}]\cr
#   Destination path where the XML file should be saved.
# @return [\code{invisible(NULL)}].
writeOMLFlowXML = function(description, file) {
  assertClass(description, "OMLFlow")
  assertPathForOutput(file, overwrite = TRUE)

  doc = newXMLDoc()
  top = newXMLNode("oml:flow", parent = doc, namespace = c(oml = "http://openml.org/openml"))

  mynode = function(name, val, parent = top){
    if (!is.na(val))
      newXMLNode(name, as.character(val), parent = parent, namespace = "oml")
  }

  addNodes = function(description, doc, parent = top) {
    mynode("name", description$name, parent)
    mynode("external_version", description$external.version, parent)
    mynode("description", description$description, parent)
    mynode("creator", description$creator, parent)
    mynode("contributor", description$contributor, parent)
    mynode("licence", description$licence, parent)
    mynode("language", description$language, parent)
    mynode("full_description", description$full.description, parent)
    mynode("installation_notes", description$installation.notes, parent)
    mynode("dependencies", description$dependencies, parent)

    for (i in seq_along(description$bibliographical.reference)) {
      par = newXMLNode("bibliographical_reference", parent = parent, namespace = "oml")
      mynode("citation", description$bibliographical.reference[[i]]$citation, parent = par)
      mynode("url", description$bibliographical.reference[[i]]$url, parent = par)
    }
    for (i in seq_along(description$parameters)) {
      par = newXMLNode("parameter", parent = parent, namespace = "oml")
      mynode("name", description$parameters[[i]]$name, parent = par)
      mynode("data_type", description$parameters[[i]]$data.type, parent = par)
      mynode("default_value", description$parameters[[i]]$default.value, parent = par)
      mynode("description", description$parameters[[i]]$description, parent = par)
    }
    # for (i in seq_along(description$components)) {
    #   comp = newXMLNode("component", parent = top, namespace = "oml")
    #   identifier = names(description$components)[i]
    #   identifier = ifelse(!is.null(identifier), identifier, description$components[[i]]$name)
    #   mynode("identifier", identifier, parent = comp)
    #   sub.impl = newXMLNode("flow", parent = comp, namespace = "oml")
    #   doc = addNodes(description$components[[i]], doc, parent = sub.impl)
    # }
    mynode("source_format", description$source.format, parent)
    mynode("binary_format", description$binary.format, parent)
    mynode("source_md5", description$source.md5, parent)
    mynode("binary_md5", description$binary.md5, parent)
    return(doc)
  }

  doc = addNodes(description, doc, top)
  saveXML(top, file = file)
}
