\name{runTask}
\alias{runTask}
\title{Solve a task with MLR.}
\usage{
  runTask(task, learner, return.mlr.results = FALSE,
    remove.const.feats = TRUE, ...)
}
\arguments{
  \item{task}{[\code{\linkS4class{OpenMLTask}}]\cr An
  OpenML task. Required.}

  \item{learner}{[\code{\link[mlr]{Learner}}]\cr Learner
  object from package mlr to solve the task.}

  \item{return.mlr.results}{[\code{logical(1)}]\cr Should
  not only the predictions but all of the by MLR computed
  information be returned?  This includes test measures in
  each step of the resampling procedure as well as the
  aggregated performance. See \code{\link[mlr]{resample}}.
  Default is \code{TRUE}.}

  \item{remove.const.features}{[\code{logical(1)}]\cr
  Should constant features be removed? Default is
  \code{TRUE}. Note, that setting this to \code{FALSE} may
  lead to fatal errors.}
}
\value{
  List of:
  \item{run.pred}{[\code{\link[mlr]{ResamplePrediction}}]\cr
  Predictions resulting from the run. These are necessary
  in order to upload a run.}
  \item{mlr.resample.results}{[\code{list}]\cr The results
  of the MLR function \code{\link[mlr]{resample}}.\cr
  \code{NULL} if \code{return.mlr.results == FALSE}.}
}
\description{
  Solve a task with a specified learner from MLR, compute
  predictions and upload the results to the OpenML server.
}
\seealso{
  \code{\linkS4class{OpenMLTask}},
  \code{\link[mlr]{learners}},
  \code{\link{authenticateUser}},
  \code{\link[mlr]{resample}}
}

