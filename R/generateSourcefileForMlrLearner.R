generateSourcefileForMlrLearner = function(flow, dir = getwd()) {
  assertClass(flow, "OpenMLImplementation")
  assertDirectory(dir)
  file = file.path(dir, sprintf("%s_source.R", flow$name))
  catf(file = file, "library(mlr) \nlrn = makeLearner(\"%s\")", implementation$name)
  return(file)
}