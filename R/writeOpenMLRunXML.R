# Generate an XML file for an OpenMLRun object.
#
# @param description [\code{\link{OpenMLRun}}]\cr
#   OpenML run description object.
# @param file [\code{character(1)}]\cr
#   Destination path where the XML file should be saved.
# @return [\code{invisible(NULL)}].
writeOpenMLRunXML = function(description, file) {
  assertClass(description, "OpenMLRun")
  assertPathForOutput(file, overwrite = TRUE)
  
  doc = newXMLDoc()
  top = newXMLNode("oml:run", parent = doc, namespace = c(oml = "http://openml.org/openml"))
  
  mynode = function(name, val, parent = top) {
    if (!is.na(val)) 
      newXMLNode(name, as.character(val), parent = parent, namespace = "oml")
  }
  
  mynode("task_id", description$task.id)
  mynode("implementation_id", description$implementation.id)
  mynode("error_message", description$error.message)
  
  for (i in seq_along(description$parameter.settings)) {
    par.setting = newXMLNode("parameter_setting", parent = top, namespace = "oml")
    mynode("name", description$parameter.settings[[i]]$name, parent = par.setting)
    mynode("value", description$parameter.settings[[i]]$value, parent = par.setting)
    mynode("component", description$parameter.settings[[i]]$component, parent = par.setting)
  }
  saveXML(top, file = file)
}
