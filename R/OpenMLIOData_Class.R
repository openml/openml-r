#' @title Construct OpenMLIOData.
#'   
#' @param dataset [\code{list}]\cr An optional list of one or more data set descriptions with 
#'   elements "did", "name" and "url".
#'   Default is \code{NULL}, which means there are no data sets.
#' @param evaluation [\code{data.frame}]\cr 
#'   An optional list of one or more evaluations. Default is \code{NULL}, which means there are 
#'   no data sets.
#' @export
#' @aliases OpenMLIOData
makeOpenMLIOData = function(dataset = NULL, evaluation) {
  if (!is.null(dataset))
    assertList(dataset)
  # assertDataFrame(evaluation) -> error?
  makeS3Obj("OpenMLIOData", 
    dataset = dataset,
    evaluation = evaluation
  )
}

# ***** Methods *****

# show
#' @export
print.OpenMLIOData = function(x, ...) {
  catf('\n** Data Set(s) **')
  print(do.call(rbind.data.frame, x$dataset))
  
  catf('\n** Evaluations **')
  # Exclude column "array.data" in print to keep it readable
  eval = x$evaluation
  print(eval[, colnames(eval) != "array.data"])
}
