#' @note
#' This function is memoised with a 5 minute timeout. I.e., if you call this function twice in 5 minutes,
#' the first call will query the server and store the results in memory while the second call will return
#' the cached results from the first call. You can reset the cache by calling \code{\link[memoise]{forget}}
#' on the function manually.
