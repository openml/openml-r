\name{getMetaLearningFeatures}
\alias{getMetaLearningFeatures}
\title{getMetaLearningFeatures}
\usage{
  getMetaLearningFeatures(only = NULL)
}
\arguments{
  \item{only}{[\code{character}]\cr The data qualities that
  are to be retrieved. A complete list can be obtained by
  \code{\link{getDataQualityNames}}.  Default is all data
  qualities are retrieved.}
}
\value{
  [\code{data.frame}]. A \code{data.frame} containing the
  data qualities.
}
\description{
  Retrieve meta learning features about all stored data
  sets. Basic data characteristics as obtained by
  \code{\link{getDataCharacteristics}} are also included.
}

