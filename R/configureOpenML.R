#' @title Configures the behavior of the package.
#'
#' @description
#' Configuration is done by setting custom \code{\link{options}}.
#'
#' If you do not set an option here, its current value will be kept.
#'
#' If you call this function with an empty argument list, everything is set to its defaults.
#'
#' @param show.info [\code{logical(1)}]\cr
#'   Some methods of OpenML support a \code{show.info} argument to enable
#'   verbose output on the console. This option sets the default value for these arguments.
#'   Setting the argument manually in one of these functions will overwrite the default
#'   value for that specific function call.
#'   Default is \code{TRUE}.
#' @return [\code{invisible(NULL)}].
#' @family configure
#' @export
configureOpenML = function(show.info) {
  defaults = list(show.info = TRUE)
  any.change = FALSE
  
  if (!missing(show.info)) {
    assertFlag(show.info)
    setOpenMLOption("show.info", show.info)
    any.change = TRUE
  }

  # set everything to defaults if all pars are missing
  if (!any.change)
    Map(setOpenMLOption, names(defaults), defaults)
  invisible(NULL)
}