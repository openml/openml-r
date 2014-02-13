\name{authenticateUser}
\alias{authenticateUser}
\title{Authenticate at server.}
\usage{
authenticateUser(email, password, show.info = TRUE)
}
\arguments{
  \item{email}{[\code{character(1)}]\cr Your E-mail address
  at OpenMl server.}

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

