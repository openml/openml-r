#' @title Check status of cached datasets.
#'
#' @description
#' The caching mechanism is fine, but sometimes you might want to
#' work on a dataset, which is already cached and has been deactivated in the
#' meanwhile. This function can be used to determine the status of all cached
#' datasets.
#'
#' @param show.warnings [\code{logical(1)}]\cr
#'   Show warning if there are deactivated datasets in cache?
#'   Default is \code{TRUE}.
#' @param ... \cr 
#'   Arguments passed to \code{\link{listOMLDataSets}}
#' @return [\code{data.frame}]
#' @example inst/examples/getCachedOMLDataSetStatus.R
#' @export
getCachedOMLDataSetStatus = function(show.warnings = TRUE, ...) {
  assertFlag(show.warnings)

  cache.dir = getOMLConfig()$cachedir

  dids = as.integer(list.files(paste0(cache.dir, "/datasets")))
  if (length(dids) == 0L) {
    return(data.frame())
  }

  all.ds = listOMLDataSets(...)
  cached.ds = all.ds[all.ds$did %in% dids, c("did", "status"), drop = FALSE]
  if (any(cached.ds$status == "deactivated") > 0L & show.warnings) {
    warningf("There are deactivated datasets in the cache.")
  }
  return(cached.ds)
}
