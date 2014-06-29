#' Generate a list of OpenML implementation parameters for a given mlr learner.
#' 
#' @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr
#'   The mlr learner.
#' @return A list of \code{\link{OpenMLImplementationParameter}s}.
#' @examples
#' library(mlr)
#' lrn = makeLearner("classif.randomForest")
#' pars = makeImplementationParameterList(lrn)
#' pars
#' @export
makeImplementationParameterList = function(mlr.lrn) {
  pars = mlr.lrn$par.set$pars
  par.list = vector("list", length = length(pars))
  for(i in seq_along(pars)){
    name = pars[[i]]$id
    data.type = pars[[i]]$type
    # FIXME: data.type Should be either integer, numeric, string, vector, matrix, object.
    # if(data.type == "discrete") data.type = "string"      ? 
    # if(data.type == "numericvector") data.type = "vector" ? 
    # ...
    if (pars[[i]]$has.default)
      default.value = as.character(pars[[i]]$default)
    else
      default.value = NA_character_
    impl.par = makeOpenMLImplementationParameter(
      name = name, 
      data.type = data.type, 
      default.value = default.value)
    par.list[[i]] = impl.par
  }
  return(par.list)
}