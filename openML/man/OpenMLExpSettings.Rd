\docType{class}
\name{OpenMLExpSettings}
\alias{OpenMLExpSettings}
\alias{OpenMLExpSettings-class}
\title{OpenMLExpSettings}
\description{
This class of objects contains the information on an
experimental method.
}
\details{
Objects can be created by calls of the form
\code{OpenMLExpSettings(...)}. The objects contain
information on ... .
}
\section{Slots}{
  \describe{ \item{\code{type}}{[\code{character}]\cr The
  type of the evaluation method.}
  \item{\code{n.folds}}{[\code{integer(1)}]\cr The number
  of folds of the evaluation procedure (for most methods
  this is 1, but for cross validation this will typically
  be a larger number).}
  \item{\code{n.repeats}}{[\code{integer(1)}]\cr The number
  of times the experimental procedure is to be repeated
  with different randomization.}
  \item{\code{data.splits}}{[\code{data.frame}]\cr The
  actual data splits to be used on each fold and each
  repetition. This is a data frame with columns:
  \code{type} with possible values \code{TRAIN} or
  \code{TEST}; \code{rowid}; \code{fold}; and
  \code{repeat}.} }
}
\seealso{
\code{\linkS4class{OpenMLTask}}
}

