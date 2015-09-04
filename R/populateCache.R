#' @title Download a bunch of OpenML objects to cache.
#'
#' @description Given a set of tasks ids, flow ids, run ids and/or dataset ids
#' the function populates the cache directory by downloading the corresponding
#' objects. This is of particular interest in highly parallel computations on
#' a cluster with a shared file system.
#'
#' @param task.ids [\code{integer}]\cr
#'   Vector of task ids.
#' @param flow.ids [\code{integer}]\cr
#'   Vector of flow ids.
#' @param run.ids [\code{integer}]\cr
#'   Vector of run ids.
#' @param dids [\code{integer}]\cr
#'   Vector of dataset ids.
#' @template arg_verbosity
#' @return [\code{invisible(NULL)}]
#' @export
populateOMLCache = function(task.ids = NULL, flow.ids = NULL, run.ids = NULL, dids = NULL, verbosity = NULL) {
  # sanity check passed stuff
  !is.null(task.ids) && assertInteger(task.ids, lower = 1L, min.len = 1L, any.missing = FALSE, unique = TRUE)
  !is.null(flow.ids) && assertInteger(flow.ids, lower = 1L, min.len = 1L, any.missing = FALSE, unique = TRUE)
  !is.null(run.ids) && assertInteger(run.ids, lower = 1L, min.len = 1L, any.missing = FALSE, unique = TRUE)
  !is.null(dids) && assertInteger(dids, lower = 1L, min.len = 1L, any.missing = FALSE, unique = TRUE)

  # store all that stuff in a list so we can iterate over it
  id.list = list(tasks = task.ids, flows = flow.ids, runs = run.ids, datasets = dids)
  id.list = filterNull(id.list)

  if (length(id.list) == 0L) {
    stopf("At least one of 'task.ids', 'flow.ids', 'run.ids', 'dids' must be provided.")
  }

  # Helper function to determine the download function
  getDownloader = function(type) {
    switch(type,
      tasks = getOMLTask,
      flows = getOMLFlow,
      runs = getOMLRun,
      datasets = getOMLDataSet
    )
  }

  # now go through all the ids and fill the cache
  lapply(names(id.list), function(type) {
    showInfo(verbosity, "Downloading '%s' to cache.", type)
    ids = id.list[[type]]
    downloader = getDownloader(type)
    for (id in ids) {
      downloader(id, verbosity = verbosity)
    }
  })
  return(invisible(NULL))
}


