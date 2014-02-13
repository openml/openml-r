\name{uploadOpenMLRun}
\alias{uploadOpenMLRun}
\title{Upload an OpenML run to the server.}
\usage{
uploadOpenMLRun(task, mlr.lrn, oml.impl, predictions, session.hash,
  run.pars = NULL, show.info = TRUE)
}
\arguments{
  \item{task}{[\code{\link{OpenMLTask}}]\cr The task.}

  \item{mlr.lrn}{[\code{\link[mlr]{Learner}}]\cr The mlr
  learner, if used. Optional.}

  \item{oml.impl}{[\code{\link{OpenMLImplementation}}]\cr
  The OpenML implementation object of the implementation.
  Must be stored on the OpenML server.}

  \item{predictions}{[\code{data.frame}]\cr The
  predictions. Must have the same form as produced by
  \code{\link{runTask}}.}

  \item{session.hash}{[\code{character(1)}]\cr A session
  token returned by \code{\link{authenticateUser}}.}

  \item{run.pars}{[\code{list}]\cr If no mlr learner was
  used, this is a list of all
  \code{\link{OpenMLRunParameter}s}.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}
}
\value{
[\code{numeric(1)} or \code{NULL}]. Run ID if the run was
uploaded succesfully.
}
\description{
Share a run of an implementation on a given OpenML task.
}

