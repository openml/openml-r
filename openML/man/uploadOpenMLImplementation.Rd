\name{uploadOpenMLImplementation}
\alias{uploadOpenMLImplementation}
\title{Upload an OpenML implementation to the server.}
\usage{
  uploadOpenMLImplementation(description, sourcefile,
    binaryfile, session.hash, show.info = TRUE)
}
\arguments{
  \item{description}{[\code{\link{OpenMLImplementation}}]\cr
  An OpenMLImplementation object. Should at least contain a
  name and a description.}

  \item{sourcefile}{[\code{character(1)}]\cr The source
  code of the implementation. If multiple files, please zip
  them.  Either source or binary is required.}

  \item{binaryfile}{[\code{character(1)}]\cr The binary of
  the implementation. If multiple files, please zip them.
  Either source or binary is required.}

  \item{session.hash}{[\code{character(1)}]\cr A session
  token returned by \code{\link{authenticateUser}}.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}
}
\description{
  Upload an OpenML implementation to the server.
}

