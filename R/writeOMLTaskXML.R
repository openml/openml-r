# Generate an XML file for an OpenMLTask object.

writeOMLTaskXML = function(task_type_id, source_data, target_feature, estimation_procedure,
                           file, evaluation_measures = NULL) {
  
  assertClass(task_type_id, "integer")
  assertClass(source_data, "integer")
  assertClass(target_feature, "character")
  assertClass(estimation_procedure, "integer")
  assertPathForOutput(file, overwrite = TRUE)
  
  doc = newXMLDoc()
  top = newXMLNode("oml:task_inputs", parent = doc, namespace = c(oml = "http://openml.org/openml"))
  
  mynode = function(name, val, parent = top, attrs = NULL){
    if (!is.na(val))
      newXMLNode(name, as.character(val), parent = parent, namespace = "oml", attrs = attrs)
  }
  mynode(name = "task_type_id", val = task_type_id, parent = top)
  mynode(name = "input", val = source_data, parent = top, attrs = list(name = "source_data"))
  mynode(name = "input", val = target_feature, parent = top, attrs = list(name = "target_feature"))
  mynode(name = "input", val = estimation_procedure, parent = top, attrs = list(name = "estimation_procedure"))
  
  if(!is.null(evaluation_measures)) {
    assertClass(evaluation_measures, "character")
    mynode("evaluation_measures", evaluation_measures, top)
  }
  
  saveXML(top, file = file)
}
