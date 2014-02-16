\name{getDataQualities}
\alias{getDataQualities}
\title{Retrieve various data quality measures that describe a data set.}
\usage{
  getDataQualities(set = "basic")
}
\arguments{
  \item{set}{[\code{character(1)}]\cr Either
  \code{"basic"}, which means only rudimentary data
  qualities (number of features/instances/classes/missing
  values/...)  are retrieved, or \code{"all"}. The latter
  includes 'basic' data qualities as well as meta learning
  features.  Default is \code{"basic"}.}
}
\value{
  [\code{data.frame}]. Rows correspond to data sets,
  columns to data qualities.
}
\description{
  Useful if you want to get a first impression of the
  available data sets or select appropriate data sets for a
  certain study.
}
\details{
  For a complete list of all data qualities, see
  \code{\link{getDataQualityNames}}.
}

