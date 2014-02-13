\name{createOpenMLImplementationForMLRLearner}
\alias{createOpenMLImplementationForMLRLearner}
\title{createOpenMLImplementationForMLRLearner.}
\usage{
createOpenMLImplementationForMLRLearner(lrn, name = lrn$id, version,
  description)
}
\arguments{
  \item{lrn}{[\code{\link[mlr]{Learner}}]\cr The mlr
  learner.}

  \item{name}{[\code{character(1)}]\cr The name of the
  implementation object. Default is the learner's ID.}

  \item{version}{[\code{character(1)}]\cr The version of
  the implementation. Default is the version of the
  package.}

  \item{description}{[\code{character(1)}]\cr An optional
  description of the learner.  Default is a short
  specification of the learner and the associated package.}
}
\value{
[\code{\link{OpenMLImplementation}}].
}
\description{
Create an OpenML implementation description object for an
mlr learner. Required if you want to upload an mlr learner.
}

