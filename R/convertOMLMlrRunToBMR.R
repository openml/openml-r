#' @title Convert \code{OMLMlrRun}s to a \code{BenchmarkResult}.
#'
#' @description
#' Converts one or more \code{\link{OMLMlrRun}}s to a \code{\link[mlr]{BenchmarkResult}}.
#'
#' @param ... [\code{\link{OMLMlrRun}}]\cr
#'   One or more \code{\link{OMLMlrRun}s}
#' @return [\code{\link[mlr]{BenchmarkResult}}].
#' @family run-related functions
#' @export
convertOMLMlrRunToBMR = function(...) {
  set = list(...)
  assertList(set, types = "OMLMlrRun")
  bmr = lapply(set, function(x) x$bmr)
  return(do.call(mergeBMR, bmr))
}

# FIXME: helper function, replace by mergeBenchmarkResult from mlr once this is finished
mergeBMR = function(...) {
  set = list(...)
  assertList(set, types = "BenchmarkResult")

  # get number of benchmark experiments of all passed BenchmarkResults
  n.exp = viapply(set, function(X) nrow(unique(as.data.frame(X)[, c("task.id", "learner.id")])))
  #sapply(set, function(X) length(capture.output(print(X), file = NULL))-1)
  # get the index of BenchmarkResult objects that contain more than one experiment
  ind = which(n.exp!=1)
  if (length(ind)!=0) stopf("The 'BenchmarkResult' object(s) passed at position %s must contain only one experiment", collapse(ind))

  # get task and learner names from BenchmarkResult objects
  task.names = vcapply(set, function(X) names(X$results))
  learner.names = vcapply(set, function(X) names(X$learners))
  # all possible combos
  all.combos = expand.grid(unique(task.names), unique(learner.names))
  # all combos that are contained in the passed BenchmarkResult objects
  existing.combos = data.frame(task.names, learner.names)

  # get missing combos
  diff = setdiff(apply(all.combos, 1, collapse, " with learner "),
    apply(existing.combos, 1, collapse, " with learner "))
  if (length(diff)!=0)
    stopf("The following Task-Learner combination are missing: \n-%s\n", collapse(diff, "\n-"))

  # split BenchmarkResult objects by tasks and combine all learners that were computed on each tasks
  split.by.tasks = split(set, as.factor(task.names))
  merged.by.tasks = lapply(split.by.tasks, function(X) Reduce(function(...) mlr::mergeBenchmarkResultLearner(...), X))
  # merge all experiments
  merged = Reduce(function(...) mlr::mergeBenchmarkResultTask(...), merged.by.tasks)
  return(merged)
}
