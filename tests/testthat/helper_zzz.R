library(mlr)
library(stringi)
library(checkmate)

setOMLConfig(verbosity = 0L)
# if on travis, use our encrypted key, which is now in an OS envir var
if (identical(Sys.getenv("TRAVIS"), "true")) {
  apikey = Sys.getenv("OPENMLAPIKEY")
  setOMLConfig(apikey = apikey)
}
configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)
