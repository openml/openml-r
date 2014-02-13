\docType{class}
\name{OpenMLImplementationParameter}
\alias{OpenMLImplementationParameter}
\alias{OpenMLImplementationParameter-class}
\title{OpenMLImplementationParameter}
\description{
This class of objects contains information on an
implementation parameter.
}
\details{
Objects can be created by calls of the form
\code{OpenMLImplementationParameter(...)}. The objects
contain information on ... .
}
\section{Slots}{
  \describe{ \item{\code{name}}{[\code{character}]\cr The
  name of the parameter.}
  \item{\code{data.type}}{[\code{character}]\cr The data
  type of the parameter. Should be either integer, numeric,
  string, vector, matrix, object.}
  \item{\code{default.value}}{[\code{character}]\cr The
  default value of the parameter. Optional, but highly
  encouraged.}
  \item{\code{description}}{[\code{character}]\cr A
  description of what this parameter does.} }
}

