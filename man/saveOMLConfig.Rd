% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveOMLConfig.R
\name{saveOMLConfig}
\alias{saveOMLConfig}
\title{Saves a list of OpenML configuration settings to file.}
\usage{
saveOMLConfig(
  server = NULL,
  verbosity = NULL,
  apikey = NULL,
  cachedir = NULL,
  arff.reader = NULL,
  confirm.upload = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{server}{[\code{character(1)}]\cr
URL of the XML API endpoint.}

\item{verbosity}{[\code{integer(1)}]\cr
Verbosity level. Possible values are 0 (normal output), 1 (info output),
2 (debug output).}

\item{apikey}{[\code{character(1)}]\cr
Your OpenML API key. Log in to OpenML, move to your profile to get it.}

\item{cachedir}{[\code{character(1)}]\cr
Path to the cache directory.}

\item{arff.reader}{[\code{character(1)}]\cr
Name of the package which should be used to parse arff files. Possible are
\dQuote{RWeka}, which is the default and \dQuote{farff}.}

\item{confirm.upload}{[\code{logical(1)}]\cr
Should the user be asked for confirmation before upload of OML objects?}

\item{overwrite}{[\code{logical(1)}]\cr
Should an existing file be overwritten?
Default is \code{FALSE}.}
}
\description{
The new configuration is automatically assigned via
\code{\link{setOMLConfig}} if all checks pass. If you don't set a certain option,
package defaults will be inserted into the file.
}
\seealso{
Other config: 
\code{\link{configuration}},
\code{\link{getOMLConfig}()},
\code{\link{loadOMLConfig}()},
\code{\link{setOMLConfig}()}
}
\concept{config}
