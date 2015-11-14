library(mlr)
library(stringi)
library(checkmate)

# please no annoying messages, and dont use the user's cache dir
setOMLConfig(verbosity = 1L, cachedir = tempdir(), arff.reader = "RWeka")

# if on travis, use our encrypted key, which is now in an OS envir var
if (identical(Sys.getenv("TRAVIS"), "true")) {
  apikey = Sys.getenv("OPENMLAPIKEY")
  setOMLConfig(apikey = apikey)
}

configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)

# get a list of tasks, whose ids are used for testing
tasks = listOMLTasks()
tasks = tasks[with(tasks, NumberOfInstances < 1000 & NumberOfFeatures < 1000 &
  (NumberOfSymbolicFeatures==0 | is.na(NumberOfSymbolicFeatures))), ]

# try different runs of different task types
task.ids = split(tasks$task_id, tasks$task_type)
task.ids = lapply(task.ids, function(X) head(X, 3))

task.clean = lapply(unlist(task.ids), function(X) {
  r = try(listOMLRuns(task.id = X), silent = TRUE)
  if (!is.error(r)) return(r) else return(NULL)
} )
task.clean = sapply(filterNull(task.clean), function(X) head(X$run.id, 3))

