\name{uploadOpenMLRun}
\alias{uploadOpenMLRun}
\title{Upload an OpenML run to the server.}
\usage{
  uploadOpenMLRun(run.desc, predictions, session.hash,
    show.info = TRUE)
}
\arguments{
  \item{description}{[\code{\link{OpenMLRun}}]\cr An OpenML
  run description file. Should contain the task and
  implementation id and optionally any parameter settings
  that are specific for this run.}

  \item{predictions}{[\code{data.frame}]\cr A data.frame
  with the predictions. Must have the same form as produced
  by \code{\link{runTask}}.}

  \item{session.hash}{[\code{character(1)}]\cr A session
  token returned by \code{\link{authenticateUser}}.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}
}
\value{
  [\code{\numeric(1)} or \code{NULL}]. Run ID if the run
  was succesfully uploaded.
}
\description{
  Share a run of an implementation on a given OpenML task.
}

