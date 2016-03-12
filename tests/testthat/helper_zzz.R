library(mlr)
library(mboost)
library(stringi)
library(checkmate)
library(httr)
set_config(timeout(60))

# please no annoying messages, and dont use the user's cache dir
setOMLConfig(verbosity = 1L, cachedir = tempdir(), arff.reader = "farff")

# if on travis, use our encrypted key, which is now in an OS envir var
# furthermore, use a cached directory on travis
if (identical(Sys.getenv("TRAVIS"), "true")) {
  p = normalizePath("~/.openml/cache", mustWork = FALSE)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  setOMLConfig(apikey = Sys.getenv("OPENMLAPIKEY"), cachedir = p)
}

configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)

# get a list of tasks, whose ids are used for testing
tasks = listOMLTasks()
tasks = tasks[with(tasks, NumberOfInstances < 1000 & NumberOfFeatures < 1000 &
  (NumberOfSymbolicFeatures==0 | is.na(NumberOfSymbolicFeatures))), ]

# try different runs of different task types
task.ids = split(tasks$task.id, tasks$task.type)
task.ids = lapply(task.ids, function(X) head(X, 3))

task.clean = lapply(unlist(task.ids), function(X) {
  r = try(listOMLRuns(task.id = X), silent = TRUE)
  if (!is.error(r)) return(r) else return(NULL)
} )
task.clean = sapply(filterNull(task.clean), function(X) head(X$run.id, 3))

