\name{makeRunParameterList}
\alias{makeRunParameterList}
\title{Generate a list of OpenML run parameter settings for a given MLR learner.}
\usage{
  makeRunParameterList(learner, component = character(0))
}
\arguments{
  \item{learner}{[\code{\link[mlr]{Learner}}]\cr An MLR
  learner object.}

  \item{component}{[\code{character}]\cr If the learner is
  a (sub-)component of an implementation, this component's
  name.}
}
\value{
  A list of \code{\link{OpenMLRunParameter}s}.
}
\description{
  Generate a list of \code{\link{OpenMLRunParameter}s} for
  a given MLR learner.
}
\examples{
library(mlr)
lrn <- makeLearner("classif.rpart", minsplit = 1)
bagging <- makeBaggingWrapper(lrn, bag.iters = 500)

lrn.par.settings <- makeRunParameterList(lrn)
lrn.par.settings

bagging.par.settings <- makeRunParameterList(bagging)
bagging.par.settings
}

