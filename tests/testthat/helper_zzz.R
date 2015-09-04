library(mlr)
library(stringi)
library(checkmate)

# please no annoying messages, and dont use the user's cache dir
setOMLConfig(verbosity = 1L, cachedir = tempdir())

# if on travis, use our encrypted key, which is now in an OS envir var
if (identical(Sys.getenv("TRAVIS"), "true")) {
  apikey = Sys.getenv("OPENMLAPIKEY")
  setOMLConfig(apikey = apikey)
} else {
  source("../../OPENMLAPIKEY.R")
}
configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)
