#' @title Get predictions of an OpenML run.
#'
#' @description
#' This function will return the predictions of a certain OpenML run.
#'
#' @param run [\code{\link{OMLRun}}]\cr
#'   The OpenML run.
#' @template arg_verbosity
#' @return [\code{data.frame}].
#' @seealso \code{\link{OMLRun}}, \code{\link{getOMLRun}}
#' @export
getOMLPredictions = function(run, verbosity = NULL) {
  assertClass(run, "OMLRun")
  
  if (is.null(run$predictions)) {
    f = findCachedRun(run$run.id)
    if (!f$predictions.arff$found) {
      fls = run$output.data$files
      url = fls[fls$name == "predictions", "url"]
      if (is.null(url)) {
        stop("No URL found to retrieve predictions from.")
      }
      pred = downloadARFF(url, f$predictions.arff$path, verbosity)
    } else {
      showInfo(verbosity, "Predictions found in cache.")
      pred = arff.reader(f$predictions.arff$path)
    }
    return(pred)
  } else {
    return(run$predictions)
  }
}
