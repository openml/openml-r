#' @title OMLIOData.
#'
#' @description Objects of class \code{OMLIOData} are used to return information about all inputs
#'   and outputs of \code{\link{OMLRun}s}. Each object may contain up to three \code{data.frames},
#'   containing information on \code{datasets} ("data.id", "name", "url"), \code{files} ("data.id", "name", "url")
#'   and/or \code{evaluations}.
#' @name OMLIOData
#' @keywords internal
NULL

makeOMLIOData = function(datasets = NULL, files = NULL, evaluations = NULL) {
  if (!is.null(datasets))
    assertDataFrame(datasets)
  if (!is.null(files))
    assertDataFrame(files)
  if (!is.null(evaluations))
    assertDataFrame(evaluations)
  makeS3Obj("OMLIOData",
    datasets = datasets,
    files = files,
    evaluations = evaluations
  )
}

#' @export
print.OMLIOData = function(x, ...) {
  catf('\n** Data Sets **')
  print(x$datasets)
  catf('\n** Files **')
  print(x$files)
  catf('\n** Evaluations **')
  # Exclude column "array.data" in print to keep it readable
  eval = x$evaluations
  print(eval[, colnames(eval) != "array.data"])
}
