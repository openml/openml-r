% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertOMLFlowToMlr.R
\name{convertOMLFlowToMlr}
\alias{convertOMLFlowToMlr}
\title{Converts a flow to a mlr learner.}
\usage{
convertOMLFlowToMlr(flow)
}
\arguments{
\item{flow}{[\code{\link{OMLFlow}}]\cr
The flow object.}
}
\value{
[\code{\link[mlr]{Learner}}].
}
\description{
Converts an \code{\link{OMLFlow}} that was originally created with the OpenML
R-package to a \code{\link[mlr]{Learner}}.
}
\seealso{
Other flow-related functions: 
\code{\link{deleteOMLObject}()},
\code{\link{getOMLFlow}()},
\code{\link{listOMLFlows}()},
\code{\link{makeOMLFlowParameter}()},
\code{\link{makeOMLFlow}()},
\code{\link{tagOMLObject}()}
}
\concept{flow-related functions}
