#' @note
#' This function is memoised. I.e., if you call this function twice in a running R session,
#' the first call will query the server and store the results in memory while the second and all subsequent calls will return
#' the cached results from the first call.
#' You can reset the cache by calling \code{\link[memoise]{forget}} on the function manually.
