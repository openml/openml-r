#' Generate a list of OpenML run parameter settings for a given mlr learner.
#'
#' @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr
#'   The mlr learner.
#' @param component [\code{character}]\cr
#'   If the learner is a (sub-)component of an implementation, this component's name.
#' @return A list of \code{\link{OpenMLRunParameter}s}.
#' @examples
#' library(mlr)
#' lrn = makeLearner("classif.rpart", minsplit = 1)
#' bagging = makeBaggingWrapper(lrn, bw.iters = 500)
#'
#' lrn.par.settings = makeRunParameterList(lrn)
#' lrn.par.settings
#'
#' bagging.par.settings = makeRunParameterList(bagging)
#' bagging.par.settings
#' @export
makeRunParameterList = function(mlr.lrn, component = NA_character_) {
  assertClass(mlr.lrn, "Learner")
  assertString(component, na.ok = TRUE)

  par.vals = mlr.lrn$par.vals
  par.names = names(mlr.lrn$par.vals)
  par.settings = vector("list", length(par.vals))
  for (i in seq_along(par.vals)) {
    par.settings[[i]] = makeOpenMLRunParameter(
      name = par.names[i],
      value = as.character(par.vals[[i]]),
      component = component)
  }
  if (!is.null(mlr.lrn$next.learner)) {
    # Use the learner's id (without "classif." or "regr.") as the subcomponent's name...
    # FIXME: check if or make sure that this is correct
    component = strsplit(mlr.lrn$next.learner$id, split = ".", fixed = TRUE)[[1]][2]
    inner.par.settings = makeRunParameterList(mlr.lrn$next.learner, component = component)
    par.settings = c(par.settings, inner.par.settings)
  }
  return(par.settings)
}
