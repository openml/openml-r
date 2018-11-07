# Generate an XML file for an OpenMLDataSetDescription object.
#
# @param description [\code{\link{OMLDataSetDescription}}]\cr
#   OpenML dataset description object.
# @param file [\code{character(1)}]\cr
#   Destination path where the XML file should be saved.
# @return [\code{invisible(NULL)}].
writeOMLDataSetXML = function(description, file) {
  assertClass(description, "OMLDataSetDescription")
  assertPathForOutput(file, overwrite = TRUE)

  doc = newXMLDoc()
  top = newXMLNode("oml:data_set_description", parent = doc, namespace = c(oml = "http://openml.org/openml"))

  mynode = function(name, val, parent = top){
    if (!is.na(val))
      newXMLNode(name, as.character(val), parent = parent, namespace = "oml")
  }

  default.target.attribute = collapse(description$default.target.attribute)
  ignore.attribute = collapse(description$ignore.attribute)

  addNodes = function(description, doc, parent = top) {
    mynode("name", description$name, parent)
    mynode("version", description$version, parent)
    mynode("description", description$description, parent)
    mynode("format", description$format, parent)
    mynode("creator", description$creator, parent)
    mynode("contributor", description$contributor, parent)
    mynode("collection_date", description$collection.date, parent)
    mynode("language", description$language, parent)
    mynode("licence", description$licence, parent)
    mynode("default_target_attribute", default.target.attribute, parent)
    mynode("row_id_attribute", description$row.id.attribute, parent)
    mynode("ignore_attribute", ignore.attribute, parent)
    mynode("citation", description$citation, parent)
    mynode("original_data_url", description$original.data.url, parent)
    mynode("paper_url", description$paper.url, parent)
    mynode("md5_checksum", description$md5.checksum, parent)
    return(doc)
  }

  doc = addNodes(description, doc, top)

  saveXML(top, file = file)
}
