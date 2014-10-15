generateSourcefileForMlrLearner = function(lrn, dir = getwd()) {
  assertClass(lrn, "Learner")
  assertDirectory(dir)
  requirePackages("base64enc", why = "generating a sourcefile")
  file = file.path(dir, sprintf("%s_source.R", lrn$id))
  xx = base64encode(serialize(lrn, ascii = TRUE, connection = NULL))
  text = c("sourcedFlow = function(task.id) {",
           "  requirePackages(c('base64enc', 'mlr'), why = 'using the sourcefile')",
           "  task = downloadOpenMLTask(task.id)",
   sprintf("  lrn = unserialize(base64decode('%s'))", xx),
           "  runTask(task, lrn)",
           "}")
  writeLines(text, file)
  return(file)
}