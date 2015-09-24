library(mlr)
library(stringi)
library(checkmate)

# please no annoying messages, and dont use the user's cache dir
setOMLConfig(verbosity = 1L, cachedir = tempdir())

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
