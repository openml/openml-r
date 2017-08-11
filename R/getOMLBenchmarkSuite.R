#' @title OpenML task collections for benchmarking experiments.
#'
#' @description
#' A collection of 100 OpenML tasks (datasets + data splits + evaluation measure) that can be used for benchmarking experiments.
#'
#' @param name [\code{character(1)}]\cr
#'   The name of the benchmark suite
#' @template arg_verbosity
#' @return [\code{numeric}].
#' @export
getOMLBenchmarkSuite = function(name = "OpenML100", verbosity = NULL) {
  assertCharacter(name)
  if (name != "OpenML100")
    stop("Currently, only the benchmark suite 'OpenML100' is available.")
  prev = as.list(getOMLConfig())
  # reset config after exiting
  on.exit({
    do.call(setOMLConfig, prev)
  })
  # make sure to use the main server
  setOMLConfig(server = "https://www.openml.org/api/v1")
  study = getOMLStudy(14, verbosity = verbosity)
  return(study$task$task.id)
}
