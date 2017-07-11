#' @title Get suggested OpenML Dataset IDs for benchmarking experiments
#'
#' @description
#' A collection of 100 datasets that can be used for benchmarking experiments.
#'
#' @template arg_verbosity
#' @return [\code{numeric}].
#' @export
getOMLBenchmarkDataSetIds = function(verbosity = NULL) {
  prev = as.list(getOMLConfig())
  # reset config after exiting
  on.exit({
    do.call(setOMLConfig, prev)
  })
  # make sure to use the main server
  setOMLConfig(server = "https://www.openml.org/api/v1")
  study = getOMLStudy(14, verbosity = verbosity)
  return(study$data$data.id)
}

#' @title Get suggested OpenML Task IDs for benchmarking experiments
#'
#' @description
#' A collection of 100 tasks that can be used for benchmarking experiments.
#'
#' @template arg_verbosity
#' @return [\code{numeric}].
#' @export
getOMLBenchmarkTaskIds = function(verbosity = NULL) {
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
