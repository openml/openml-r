\name{OpenMLImplementation}
\alias{OpenMLImplementation}
\alias{OpenMLImplementation-class}
\title{OpenMLImplementation}
\description{
  This class of objects contains the information describing
  an openML implementation.
}
\details{
  Objects can be created by calls of the form
  \code{OpenMLImplementation(...)}. The objects contain
  information on ... .
}
\section{Slots}{
  \describe{ \item{\code{id}}{[\code{character}]\cr The id
  of the implementation.}
  \item{\code{name}}{[\code{character}]\cr The name of the
  implementation. Name-version combinations should be
  unique.} \item{\code{version}}{[\code{character}]\cr The
  version of the implementation.}
  \item{\code{description}}{[\code{character}]\cr A user
  description of the implementation. }
  \item{\code{creator}}{[\code{character}]\cr Optional. The
  persons/institutions that created the implementation.}
  \item{\code{contributor}}{[\code{character}]\cr Optional.
  (Minor) contributors to the workflow}
  \item{\code{licence}}{[\code{character}]\cr Optional.
  Default is none, meaning Public Domain or "don't
  know/care".} \item{\code{language}}{[\code{character}]\cr
  Optional. Starts with one upper case letter, rest is
  lower case. Default is English.}
  \item{\code{full.description}}{[\code{character}]\cr
  Optional. Full description of the workflow, e.g, man
  pages filled in by tool.  This is a much more elaborate
  description than given in the 'description field'. It may
  include information about all components of the
  workflow.} \item{\code{date}}{[\code{character}]\cr Time
  information on the implementation. Default is
  \code{Sys.time()}.}
  \item{\code{installation.notes}}{[\code{character}]\cr
  Installation notes.}
  \item{\code{dependencies}}{[\code{character}]\cr The
  dependencies of the implementation.}
  \item{\code{parameter}}{[\code{list}]\cr The parameters
  of the implementation. A list contatining
  \code{\link{OpenMLImplementationParameter}s}.}
  \item{\code{source.url}}{[\code{character}]\cr URL of the
  source file.}
  \item{\code{binary.url}}{[\code{character}]\cr URL of the
  binary file.}
  \item{\code{binary.format}}{[\code{character}]\cr Format
  of the binary file.}
  \item{\code{source.format}}{[\code{character}]\cr Format
  of the source file.}
  \item{\code{source.md5}}{[\code{character}]\cr MD5
  checksum to check if the source code was uploaded
  correctly.}
  \item{\code{binary.md5}}{[\code{character}]\cr MD5
  checksum to check if the binary code was uploaded
  correctly.} }
}

