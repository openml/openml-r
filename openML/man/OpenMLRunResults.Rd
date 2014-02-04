\name{OpenMLRunResults}
\alias{OpenMLRunResults}
\alias{OpenMLRunResults-class}
\title{OpenMLRunResults}
\description{
  This class of objects contains information on the results
  of an \code{\link{OpenMLRun}}.
}
\details{
  The objects contain information on ... .
}
\section{Slots}{
  \describe{ \item{\code{run.id}}{[\code{character}]\cr ID
  of the OpenMLRun.}
  \item{\code{task.id}}{[\code{character}]\cr ID of the
  OpenMLTask that belongs to this run.}
  \item{\code{user.id}}{[\code{character}]\cr The
  uploader's OpenML user ID.}
  \item{\code{implementation.id}}{[\code{character}]\cr ID
  of the implementation used for this run.}
  \item{\code{parameters}}{[\code{character}]\cr A named
  character vector of the parameter settings used for this
  run (if they differ from the default settings).}
  \item{\code{metrics}}{[\code{data.frame}]\cr A list of
  the metrics that are computed by the server based on this
  run's predictions.} }
}

