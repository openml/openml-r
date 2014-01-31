\name{makeImplementationParameterList}
\alias{makeImplementationParameterList}
\title{Generate a list of OpenML implementation parameters for a given MLR learner.}
\usage{
makeImplementationParameterList(learner)
}
\arguments{
  \item{learner}{[\code{\link[mlr]{Learner}}]\cr An MLR
  learner object.}
}
\value{
A list of \code{\link{OpenMLImplementationParameter}s}.
}
\description{
Generate a list of
\code{\link{OpenMLImplementationParameter}s} for a given
MLR learner.
}
\examples{
library(mlr)
lrn <- makeLearner("classif.randomForest")
pars <- makeImplementationParameterList(lrn)
pars
}

