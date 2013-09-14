\name{authenticateUser}
\alias{authenticateUser}
\title{Authenticate at server.}
\usage{
  authenticateUser(username, password, show.info = TRUE)
}
\arguments{
  \item{username}{[\code{character(1)}]\cr Your username /
  email at OpenML server.}

  \item{password}{[\code{character(1)}]\cr Your password at
  OpenML server.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}
}
\value{
  [\code{character(1)}]. Session hash for further
  communication.
}
\description{
  Required if you want to upload anything.
}

