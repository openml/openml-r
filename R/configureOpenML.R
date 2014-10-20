#' @title Configures the behavior of the package.
#'
#' @description
#' Configuration is done by setting custom \code{\link{options}}.
#' If you do not set an option here, its current value will be kept.
#'
#' @param show.info [\code{logical(1)}]\cr
#'   Some methods of OpenML support a \code{show.info} argument to enable
#'   verbose output on the console. This option sets the default value for these arguments.
#'   Setting the argument manually in one of these functions will overwrite the default
#'   value for that specific function call.
#'   Default is \code{TRUE}.
#' @return [\code{named list}] (invisibly) with updated configuration.
#' @family configure
#' @export
#' @examples
#'  # save state
#'  before = configureOpenML()
#'  configureOpenML(show.info = FALSE)
#'  # reset
#'  do.call(configureOpenML, before)
configureOpenML = function(show.info, cache.dir) {
  if (!missing(show.info)) {
    assertFlag(show.info)
    setOpenMLOption("show.info", show.info)
  }
  if (!missing(cache.dir)) {
    assertString(cache.dir)
    if (!isDirectory(cache.dir)) {
      if (!dir.create(cache.dir))
        stopf("Could not create cache directory '%s'", cache.dir)
    }
    setOpenMLOption("cache.dir", cache.dir)
  }

  invisible(getOpenMLOptions())
}
