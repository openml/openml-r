\name{OpenMLTask}
\alias{OpenMLTask}
\alias{OpenMLTask-class}
\title{OpenMLTask}
\description{
  This class of objects contains the information describing
  an openML task.
}
\details{
  Objects can be created by calls of the form
  \code{OpenMLTask(...)}. The objects contain information
  on ... .
}
\section{Slots}{
  \describe{ \item{\code{task.id}}{[\code{integer(1)}]\cr
  The task's OpenML ID.}
  \item{\code{task.type}}{[\code{character}]\cr The task's
  type.} \item{\code{task.pars}}{[\code{list}]\cr A set of
  parameters specific to the task type.}
  \item{\code{task.target.features}}{[\code{character}]\cr
  The name(s) of the target feature(s)}
  \item{\code{task.data.desc.id}}{[\code{integer(1)}]\cr
  The OpenML ID of the data set associated with the task.}
  \item{\code{task.data.desc}}{[\code{\link{OptionalOpenMLDataSetDescription}}]\cr
  Information on the data set.}
  \item{\code{task.estimation.procedure}}{[\code{\link{OptionalOpenMLEstimationProcedure}}]\cr
  Information on the task's estimation method and the
  asoociated data splits.}
  \item{\code{task.preds}}{[\code{list}]\cr A list that
  contains information on the format of the predictions for
  the particular task.}
  \item{\code{task.evaluation.measures}}{[\code{character}]\cr
  The evaluation measure(s) that should be used for
  optimization.} }
}
\examples{
showClass("OpenMLTask")
}
\seealso{
  \code{\linkS4class{OpenMLDataSetDescription}}
}

