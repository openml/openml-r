#' @title Construct OMLRunParList.
#' 
#' @description
#' Generate a list of OpenML run parameter settings for a given mlr learner.
#'
#' @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr
#'   The mlr learner.
#' @param component [\code{character}]\cr
#'   If the learner is a (sub-)component of a flow, this component's name.
#' @return A \code{OMLRunParList} which is a list of \code{\link{OMLRunParameter}s}.
#' @export
#' @aliases OMLRunParList
# @examples
# library(mlr)
# lrn = makeLearner("classif.rpart", minsplit = 1)
# bagging = makeBaggingWrapper(lrn, bw.iters = 500)
#
# lrn.par.settings = makeRunParameterList(lrn)
# lrn.par.settings
#
# bagging.par.settings = makeRunParameterList(bagging)
# bagging.par.settings
# mlr.lrn = makeOversampleWrapper(makeFilterWrapper(makeLearner("classif.randomForest", mtry = 4, ntree = 500), fw.perc = 0.5), osw.rate = 1)
# mlr.lrn = makeImputeWrapper(makeLearner("classif.randomForest", mtry = 4, ntree = 500), class = imputeMedian())
# mlr.lrn = makeOversampleWrapper(makeFilterWrapper(makeLearner("classif.randomForest", mtry = 4, ntree = 500)), osw.rate = 1)
# mlr.lrn = makeLearner("classif.randomForest", mtry = 4, ntree = 500)
makeOMLRunParList = function(mlr.lrn, component = NA_character_) {
  assertClass(mlr.lrn, "Learner")
  assertString(component, na.ok = TRUE)
  
  ps = getParamSet(mlr.lrn) #mlr.lrn$par.set$pars
  par.vals = getHyperPars(mlr.lrn) # mlr.lrn$par.vals
  par.names = names(par.vals)
  # get defaults for par.vals that have been set
  par.defaults = getDefaults(ps)
  # store only par.vals that are different from default values
  par.ind = vlapply(par.names, function(x) !isTRUE(all.equal(par.defaults[[x]] , par.vals[[x]])))
  par.vals = par.vals[par.ind]
  par.names = par.names[par.ind]
  
  par.settings = setNames(vector("list", length(par.vals)), par.names)
  for (i in seq_along(par.vals)) {
    psi = ps$pars[[par.names[i]]]
    # FIXME: what happens with parameters that are vectors (or not scalars, e.g. deeplearning)?
    val = paramValueToString(psi, par.vals[[i]])
    par.settings[[i]] = makeOMLRunParameter(
      name = par.names[i],
      value = val, #par.vals[[i]],
      # FIXME: see https://github.com/openml/OpenML/issues/270
      component = component #gsub(".*[.]", "", mlr.lrn$id)
    )
  }
  
  # add component
  next.learner = mlr.lrn
  while (!is.null(next.learner)) {
    component = gsub(".*[.]", "", next.learner$id)
    par.component = intersect(names(next.learner$par.set$pars), names(par.settings))
    for (comp in par.component) {
      par.settings[[comp]]$component = component
    }
    next.learner = next.learner$next.learner
  }
  setClasses(par.settings, "OMLRunParList")
}

#' @title Construct OMLSeedParList
#' 
#' @description
#' Generate a list of OpenML seed parameter settings for a given seed.
#' 
#' @param seed [\code{numeric(1)}]\cr
#'   The seed.
#' @param prefix [\code{character}]\cr
#'   prefix for seed parameter names.
#'   
#' @return A \code{OMLSeedParList} which is a list of \code{\link{OMLRunParameter}s} 
#' that provide only information about the seed.
#' @aliases OMLSeedParList
#' @export
makeOMLSeedParList = function(seed, prefix = "openml") {
  assertIntegerish(seed)
  assert(checkString(prefix), checkNull(prefix))
  seed.pars = setNames(c(seed, RNGkind()), c("seed", "kind", "normal.kind"))
  if(!is.null(prefix))
    names(seed.pars) = paste0(prefix, ".", names(seed.pars))
  seed.setting = lapply(seq_along(seed.pars), function(x) {
    makeOMLRunParameter(
      name = names(seed.pars[x]),
      value = as.character(seed.pars[x]),
      component = NA_character_
    )
  })
  seed.setting = setNames(seed.setting, names(seed.pars))
  setClasses(seed.setting, "OMLSeedParList")
}

# show
#' @export
print.OMLRunParList = function(x, ...)  {
  #x = unclass(x)
  ret = rbindlist(lapply(x, function(x) x[c("name", "value", "component")]))
  catf("This is a '%s' with the following parameters:", class(x)[1])
  print(ret)
}

# show
#' @export
print.OMLSeedParList = function(x, ...)  {
  #x = unclass(x)
  ret = rbindlist(lapply(x, function(x) x[c("name", "value", "component")]))
  catf("This is a '%s' with the following parameters:", class(x)[1])
  ret$component = NULL
  print(ret)
}

#' @title Extract OMLSeedParList from run
#' 
#' @description
#' Extracts the seed information as \code{\link{OMLSeedParList}} from a \code{\link{OMLRun}}.
#' 
#' @param run [\code{OMLRun}]\cr
#'   A \code{\link{OMLRun}}
#'   
#' @return [\code{OMLSeedParList}].
#' @export
getOMLSeedParList = function(run) {
  assertClass(run, "OMLRun")
  par = run$parameter.setting
  seed.pars = grepl(c("seed|kind|normal.kind"), getOMLRunParListNames(run))
  assertList(par[seed.pars], len = 3)
  return(setClasses(par[seed.pars], "OMLSeedParList"))
}

#' @title Extract OMLRunParList from run
#' 
#' @description
#' Extracts the seed information as \code{\link{OMLRunParList}} from a \code{\link{OMLRun}}.
#' 
#' @param run [\code{OMLRun}]\cr
#'   A \code{\link{OMLRun}}
#'   
#' @return [\code{OMLRunParList}].
#' @export
getOMLRunParList = function(run) {
  assertClass(run, "OMLRun")
  par = run$parameter.setting
  seed.pars = grepl(c("seed|kind|normal.kind"), getOMLRunParListNames(run))
  return(setClasses(par[!seed.pars], "OMLRunParList"))
}


# hepler functions:

# @param x OMLSeedParList
setOMLSeedParList = function(x) {
  assertClass(x, "OMLSeedParList")
  seed.pars = vcapply(x, function(x) x$value)
  names(seed.pars) = c("seed", "kind", "normal.kind")
  xRNG = seed.pars[c("kind", "normal.kind")]
  
  currentRNG = RNGkind()
  if (!identical(currentRNG, unname(xRNG)))
    messagef("Kind of RNG has been changed to '%s'",
      convertToShortString(as.list(xRNG)))
  
  do.call("set.seed", as.list(seed.pars))
}

# extracts the seed value of a OMLSeedParList
extractSeed = function(x) {
  assertClass(x, "OMLSeedParList")
  seed.names = vcapply(x, function(x) x$name)
  seed = vcapply(x, function(x) x$value)[grepl("seed", seed.names)]
  as.integer(seed)
}

# get the names of a run
getOMLRunParListNames = function(run) {
  assertClass(run, "OMLRun")
  return(vcapply(run$parameter.setting, function(x) x$name))
}

# converts a OMLRunParList to a named list
convertOMLRunParListToList = function(x, ...) {
  par.list = extractSubList(x, "value")
  if(!isTRUE(checkNamed(par.list))) {
    par.names = extractSubList(x, "name")
    par.list = setNames(par.list, par.names)
  }
  return(as.list(par.list))
}
