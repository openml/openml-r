\name{OpenMLDataSetDescription}
\alias{OpenMLDataSetDescription}
\alias{OpenMLDataSetDescription-class}
\title{OpenMLDataSetDescription}
\description{
  This class of objects contains the information describing
  an openML data set.
}
\details{
  Objects can be created by calls of the form
  \code{OpenMLDataSetDescription(...)}. The objects contain
  information on ... .
}
\section{Slots}{
  \describe{ \item{\code{id}}{[\code{integer(1)}]\cr The
  OpenML ID number of the data set.}
  \item{\code{name}}{[\code{character}]\cr The name of the
  data set.} \item{\code{version}}{[\code{character}]\cr
  The version of the data set.}
  \item{\code{creator}}{[\code{character}]\cr The creator's
  name.} \item{\code{contributor}}{[\code{character}]\cr
  The contributor's name}
  \item{\code{collection.date}}{[\code{POSIXt}]\cr The date
  and the time when the data set has been collected.}
  \item{\code{upload.date}}{[\code{POSIXt}]\cr The date and
  the time when the data set has been uploaded.}
  \item{\code{description}}{[\code{character}]\cr The
  description of the data set.}
  \item{\code{language}}{[\code{character}]\cr The
  language.} \item{\code{format}}{[\code{character}]\cr The
  format of the data set.}
  \item{\code{licence}}{[\code{character}]\cr The licence
  of the data set.} \item{\code{url}}{[\code{character}]\cr
  The url of the data set.}
  \item{\code{row.id.attribute}}{[\code{character}]\cr The
  row id attribute.}
  \item{\code{md5.checksum}}{[\code{character}]\cr The md5
  checksum} \item{\code{data.set}}{[\code{character}]\cr
  The data set.} }
}

