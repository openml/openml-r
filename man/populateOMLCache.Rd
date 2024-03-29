% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/populateOMLCache.R
\name{populateOMLCache}
\alias{populateOMLCache}
\title{Download a bunch of OpenML objects to cache.}
\usage{
populateOMLCache(
  data.ids = integer(0L),
  task.ids = integer(0L),
  flow.ids = integer(0L),
  run.ids = integer(0L),
  verbosity = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{data.ids}{[\code{integer}]\cr
Dataset IDs.
Default is none.}

\item{task.ids}{[\code{integer}]\cr
Task IDs.
Default is none.}

\item{flow.ids}{[\code{integer}]\cr
Flow IDs.
Default is none.}

\item{run.ids}{[\code{integer}]\cr
Run IDs.
Default is none.}

\item{verbosity}{[\code{integer(1)}]\cr
Print verbose output on console? Possible values are:\cr
\code{0}: normal output,\cr
\code{1}: info output,\cr
\code{2}: debug output.\cr
Default is set via \code{\link{setOMLConfig}}.}

\item{overwrite}{[\code{integer(1)}]\cr
Should files that are already in cache be overwritten?}
}
\value{
[\code{invisible(NULL)}]
}
\description{
Given a set of OML object ids, the function populates the cache directory by downloading the
corresponding objects. This can avoid network access in later experiments, as you can retrieve
all objects from the cache on disk.
This is of particular interest in highly parallel computations on
a cluster with a shared file system.
}
