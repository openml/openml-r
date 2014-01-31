\name{getDataQualities}
\alias{getDataQualities}
\title{Retrieve various data quality measures that describe a data set.}
\usage{
  getDataQualities(set = "basic")
}
\arguments{
  \item{only}{[\code{character}]\cr The data qualities that
  are to be retrieved. A complete list can be obtained by
  \code{\link{getDataQualityNames}}.  Default is all data
  qualities are retrieved.}
}
\value{
  [\code{data.frame}]. Rows correspond to data sets,
  columns to data qualities.
}
\description{
  Useful if you want to get a first impression of the
  available data sets or select appropriate data sets for a
  certain study. Retrieve meta learning features about all
  stored data sets. Basic data characteristics as obtained
  by \code{\link{getDataCharacteristics}} are also
  included.
}

