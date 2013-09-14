\name{uploadOpenMLRun}
\alias{uploadOpenMLRun}
\title{Upload an OpenML run to the server.}
\usage{
  uploadOpenMLRun(description, output.files, session.hash,
    show.info = TRUE)
}
\arguments{
  \item{description}{[???]\cr An XML run description file.
  Should contain the task and implementation id and
  optionally any parameter settings that are specific for
  this run.}

  \item{output.files}{[\code{character(1)}]\cr A file
  containing the output files required by the task type
  used.  For supervised classification, this will be a
  table with predictions.}

  \item{session.hash}{[\code{character(1)}]\cr A session
  token returned by \code{\link{authenticateUser}}.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}
}
\description{
  Share a run of an implementation on a given OpenML task.
}

