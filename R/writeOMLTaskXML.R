# Generate an XML file for an OpenMLTask object.

writeOMLTaskXML = function(task.type.id, source.data, target.feature, estimation.procedure,
  file, evaluation.measures = NULL) {
  assertInt(task.type.id)
  assertInt(source.data)
  assertCharacter(target.feature)
  assertInt(estimation.procedure)
  assertPathForOutput(file, overwrite = TRUE)

  doc = newXMLDoc()
  top = newXMLNode("oml:task_inputs", parent = doc, namespace = c(oml = "http://openml.org/openml"))

  mynode = function(name, val, parent = top, attrs = NULL){
    if (!is.na(val))
      newXMLNode(name, as.character(val), parent = parent, namespace = "oml", attrs = attrs)
  }
  mynode(name = "task_type_id", val = task.type.id, parent = top)
  mynode(name = "input", val = source.data, parent = top, attrs = list(name = "source_data"))
  mynode(name = "input", val = target.feature, parent = top, attrs = list(name = "target_feature"))
  mynode(name = "input", val = estimation.procedure, parent = top, attrs = list(name = "estimation_procedure"))

  if (!is.null(evaluation.measures)) {
    assertClass(evaluation.measures, "character")
    mynode("evaluation_measures", val = evaluation.measures, parent = top)
  }
  saveXML(top, file = file)
}
