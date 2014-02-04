\name{OpenMLRun}
\alias{OpenMLRun}
\alias{OpenMLRun-class}
\title{OpenMLRun}
\description{
  This class of objects contains the information describing
  an openML run.
}
\details{
  Objects can be created by calls of the form
  \code{OpenMLRun(...)}. The objects contain information on
  ... .
}
\section{Slots}{
  \describe{ \item{\code{task.id}}{[\code{character}]\cr
  The id of the solved task.}
  \item{\code{implementation.id}}{[\code{character}]\cr The
  id of the used implementation.}
  \item{\code{parameter.settings}}{[\code{list}]\cr
  Optional parameter settings of this run. A list
  contatining \code{\link{OpenMLRunParameter}s}.} }
}

