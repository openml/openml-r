\name{makeImplementationParameterList}
\alias{makeImplementationParameterList}
\title{Generate a list of OpenML implementation parameters for a given mlr learner.}
\usage{
makeImplementationParameterList(mlr.lrn)
}
\arguments{
  \item{mlr.lrn}{[\code{\link[mlr]{Learner}}]\cr The mlr
  learner.}
}
\value{
A list of \code{\link{OpenMLImplementationParameter}s}.
}
\description{
Generate a list of OpenML implementation parameters for a
given mlr learner.
}
\examples{
library(mlr)
lrn <- makeLearner("classif.randomForest")
pars <- makeImplementationParameterList(lrn)
pars
}

