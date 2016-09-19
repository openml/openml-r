.sourcedFlowPattern = "
sourcedFlow = function(task.id) {
  library(base64enc)
  library(mlr)
  task = downloadOMLTask(task.id)
  lrn = unserialize(base64decode('%s'))
  runTask(task, lrn)
}
"

generateSourcefileForMlrLearner = function(lrn, dir = getwd()) {
  assertClass(lrn, "Learner")
  assertDirectoryExists(dir)
  fn = file.path(dir, sprintf("%s_source.R", lrn$id))
  xx = base64encode(serialize(lrn, ascii = TRUE, connection = NULL))
  writeLines(sprintf(.sourcedFlowPattern, xx), fn)
  return(fn)
}
