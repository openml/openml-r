#' @title Do chunked listings
#'
#' @description
#' Allows you to do multiple chunked requests with the listOML* functions.
#' The request will be repeated until \code{total.limit} is reached or until there are no more results available on the server.
#'
#' @param listfun [\code{character(1)}]\cr
#'  the listing function for which you want to do chunked requests.
#' @param ... [\code{ANY}]\cr
#'  arguments are passed to the function specified in \code{listfun}.
#' @param total.limit [\code{integer}]\cr
#'  the total limit of results that should be listed. Set this to a high number to get all available results from the server.
#' @param chunk.limit [\code{integer}]\cr
#'  the limit for a single request. If you reduce this number, the number of server requests will increase.
#' @family listing functions
#' @export
chunkOMLlist = function(listfun, ..., total.limit = 100000, chunk.limit = 1000) {
  assertChoice(listfun, choices = c("listOMLFlows", "listOMLRuns", "listOMLRunEvaluations", "listOMLDataSets", "listOMLTasks", "listOMLSetup"))
  args = list(...)
  assertSubset(names(args), names(formals(listfun)), empty.ok = TRUE)
  if (!is.null(args$limit) | !is.null(args$offset))
    stopf("Changing 'limit' and/or 'offset' is not allowed. 'chunkOMLlist' sets the 'limit' and 'offset' of '%s' automatically.", listfun)

  offset = seq(0, total.limit - 1, by = chunk.limit)
  res = vector("list", length(offset))
  for (i in seq_along(offset)) {
    tmp = try(do.call(listfun, args = c(list(offset = offset[i], limit = chunk.limit), args)))
    if (!(is.error(tmp) || nrow(tmp) == 0)) {
      res[[i]] = tmp
      if (i == length(offset))
        return(as.data.frame(rbindlist(filterNull(res), fill = TRUE), stringsAsFactors = FALSE))
    } else {
      return(as.data.frame(rbindlist(filterNull(res), fill = TRUE), stringsAsFactors = FALSE))
    }
  }
}
