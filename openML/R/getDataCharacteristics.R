#' Get basic data set characteristics.
#'
#' Retrieves a number of basic data characteristics like number of observations, number of features
#' and so on. If a certain characteristic is not applicable to a data set, e.g., number of classes for
#' a regression data set, its value will be \code{NA}.
#'
#' @return [\code{data.frame}]. A \code{data.frame} containing the data characteristics.
#' @export
getDataCharacteristics <- function() {
  # these are the basic features we currently get form the DB to describe a data set
  chars <- c("NumberOfFeatures", "NumberOfInstances", "NumberOfClasses", "MajorityClassSize",
    "MinorityClassSize", "NumberOfInstancesWithMissingValues", "NumberOfMissingValues",
    "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")
  data.chars <- getMetaLearningFeatures(only = chars)
  # all features (except for the name) are integers. [check this when adding new data chars]
  data.chars[, -1] <- apply(data.chars[, -1], 2, as.integer)
  return(data.chars)
}

