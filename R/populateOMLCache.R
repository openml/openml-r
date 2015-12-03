#' @title Download a bunch of OpenML objects to cache.
#'
#' @description
#' Given a set of OML object ids, the function populates the cache directory by downloading the
#' corresponding objects. This can avoid network access in later experiments, as you can retrieve
#' all objects from the cache on disk.
#' This is of particular interest in highly parallel computations on
#' a cluster with a shared file system.
#'
#' @param dids [\code{integer}]\cr
#'   Dataset ids.
#'   Default is none.
#' @param task.ids [\code{integer}]\cr
#'   Task ids.
#'   Default is none.
#' @param flow.ids [\code{integer}]\cr
#'   Flow ids.
#'   Default is none.
#' @param run.ids [\code{integer}]\cr
#'   Run ids.
#'   Default is none.
#' @template arg_verbosity
#' @param overwrite [\code{integer(1)}]\cr
#'   Should files that are already in cache be overwritten?
#' @return [\code{invisible(NULL)}]
#' @export
populateOMLCache = function(dids = integer(0L), task.ids = integer(0L),
  flow.ids = integer(0L), run.ids = integer(0L), verbosity = NULL, overwrite = FALSE) {

  # sanity check passed stuff
  task.ids = asInteger(task.ids, lower = 1L, any.missing = FALSE, unique = TRUE)
  flow.ids = asInteger(flow.ids, lower = 1L, any.missing = FALSE, unique = TRUE)
  run.ids = asInteger(run.ids, lower = 1L, any.missing = FALSE, unique = TRUE)
  dids = asInteger(dids, lower = 1L, any.missing = FALSE, unique = TRUE)

  # Helper function to dispatch to the download function
  downloadStuff = function(type, fun, ids, ...) {
    if (length(ids) > 0L) {
      showInfo(verbosity, "Downloading '%s' to cache.", type)
      lapply(ids, fun, verbosity = verbosity, overwrite = overwrite, ...)
    }
  }

  downloadStuff("datsets", downloadOMLObject, dids, object = "data")
  downloadStuff("tasks", downloadOMLObject, task.ids, object = "task")
  downloadStuff("flows", downloadOMLObject, flow.ids, object = "flow")
  downloadStuff("runs", downloadOMLObject, run.ids, object = "run")

  return(invisible(NULL))
}
