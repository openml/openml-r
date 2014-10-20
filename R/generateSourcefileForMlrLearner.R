.sourcedFlowPattern = "
sourcedFlow = function(task.id) {
  library(base64enc)
  library(mlr)
  task = downloadOpenMLTask(task.id)
  lrn = unserialize(base64decode('%s'))
  runTask(task, lrn)
}
"


generateSourcefileForMlrLearner = function(lrn, dir = getwd()) {
  assertClass(lrn, "Learner")
  assertDirectory(dir)
  file = file.path(dir, sprintf("%s_source.R", lrn$id))
  xx = base64encode(serialize(lrn, ascii = TRUE, connection = NULL))
  writeLines(sprintf(.sourcedFlowPattern, xx), file)
  return(file)
}
