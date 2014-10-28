#' @title Construct OMLEstimationProcedure.
#'
#' @param type [\code{character(1)}]\cr
#'   The type of procedure used for performance estimation: either cross-validation, holdout, bootstrap or
#'   sampling.
#' @param data.splits.url [\code{character(1)}]\cr
#'   The url from which a file with train-test splits can be downloaded.
#' @param data.splits [\code{data.frame}]\cr
#'   The train-test splits. Default is \code{NULL}, which means data splits have not yet been downloaded.
#' @param parameters [\code{list}]\cr
#'   Any parameters for the estimation procedure, e.g. the number of repeats and folds in cross-validation.
#' @export
#' @aliases OMLEstimationProcedure
makeOMLEstimationProcedure = function(type, data.splits.url = NA_character_, data.splits = NULL,
  parameters = NULL) {

  assertString(type)
  assertString(data.splits.url, na.ok = TRUE)
  if (!is.null(data.splits))
    assertDataFrame(data.splits)
  if (!is.null(parameters))
    assertList(parameters, names = "named")

  makeS3Obj("OMLEstimationProcedure",
    type = type,
    data.splits.url = data.splits.url,
    data.splits = data.splits,
    parameters = parameters
  )
}

# ***** Methods *****

# Note: The data splits and the predictions are not shown
#' @export
print.OMLEstimationProcedure = function(x, ...)  {
  catf('\nEstimation Method :: %s',x$type)
  catf('\tParameters:')
  for(i in seq_along(x$parameters)){
    if (!is.na(x$parameters[[i]]))
      catf('\t\t%s = %s', names(x$parameters)[i], x$parameters[[i]])
  }
}
