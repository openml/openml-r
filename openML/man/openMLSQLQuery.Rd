\name{openMLSQLQuery}
\alias{openMLSQLQuery}
\title{Make an SQL query.}
\usage{
  openMLSQLQuery(SQL, show.info = TRUE)
}
\arguments{
  \item{SQL}{[\code{character(1)}]\cr The SQL query in form
  of a character string.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}
}
\value{
  [\code{data.frame}]. A \code{data.frame} containing the
  results.
}
\description{
  Retrieve the results of an SQL query.
}

