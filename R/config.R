#' @title OpenML configuration.
#'
#' @description
#' After loading the package, it tries to find a configuration in your home
#' directory. The R command \code{path.expand("~/.openml/config")} gives you the
#' full path to the configuration file on your operating system.
#'
#' For further information please read the \href{https://github.com/openml/r/blob/master/doc/knitted/2-Configuration.md}{configuration section}
#' of the tutorial.
#'
#' @note
#' By default the cache directory is located in a temporary directory and
#' the cache will be deleted in between R sessions. We thus recommend to set
#' the cache directory by hand.
#'
#' @name configuration
#' @rdname configuration
#' @family config
NULL
