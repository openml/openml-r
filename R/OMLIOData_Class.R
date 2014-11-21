# @title Construct OMLIOData.
#
# @param datasets [\code{data.frame}]\cr
#   An optional \code{data.frame} with columns "did", "name" and "url".
#   Default is \code{NULL}, which means there are no data sets.
# @param files [\code{data.frame}]\cr
#   An optional \code{data.frame} with columns "did", "name" and "url".
#   Default is \code{NULL}, which means there are no files.
# @param evaluations [\code{data.frame}]\cr
#   An optional list of one or more evaluations.
#   Default is \code{NULL}, which means there are no data sets.
# @export
# @aliases OMLIOData
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
