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
# lrn.par.settings = makeOMLRunParList(lrn)
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

  if (testClass(mlr.lrn, "TuneWrapper")) mlr.lrn = removeAllHyperPars(mlr.lrn)

  ps = getParamSet(mlr.lrn)
  par.vals = mlr::getHyperPars(mlr.lrn)
  par.names = names(par.vals)
  # get defaults for par.vals that have been set
  par.defaults = getDefaults(ps)
  # store only par.vals that are different from default values
  par.ind = vlapply(par.names, function(x) !isTRUE(all.equal(par.defaults[[x]], par.vals[[x]])))
  par.vals = par.vals[par.ind]
  par.names = par.names[par.ind]

  par.settings = setNames(vector("list", length(par.vals)), par.names)
  for (i in seq_along(par.vals)) {
    psi = ps$pars[[par.names[i]]]
    val = paramToString(psi, par.vals[[i]])
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
    par.component = intersect(names(getParamSet(next.learner)$pars), names(par.settings))
    for (comp in par.component) {
      par.settings[[comp]]$component = component
    }
    next.learner = next.learner$next.learner
  }
  setClasses(par.settings, "OMLRunParList")
}

convertOMLRunParListToTable = function(x) {
  if (length(x) == 0L)
    return(data.table())
  rbindlist(lapply(x, function(x) x[c("name", "value", "component")]))
}

#' @export
print.OMLRunParList = function(x, ...)  {
  catf("This is a '%s' with the following parameters:", class(x)[1])
  print(convertOMLRunParListToTable(x))
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
  return(setClasses(par[!isSeedPar(par)], "OMLRunParList"))
}

# helpers:
# get the names of a run
# getOMLRunParListNames = function(run) {
#   assertClass(run, "OMLRun")
#   return(vcapply(run$parameter.setting, function(x) x$name))
# }

# converts a OMLRunParList to a named list
convertOMLRunParListToList = function(x, ps = NULL, ...) {
  assertClass(x, "OMLRunParList")
  par.list = extractSubList(x, "value", simplify = FALSE)
  if (!testNamed(par.list)) {
    par.names = extractSubList(x, "name")
    par.list = setNames(par.list, par.names)
  }
  # if paramset is given, convert vectors, matrices, untyped etc. properly
  if (!is.null(ps)) {
    for (i in names(x)) {
      par.list[[i]] = stringToParam(ps$pars[[i]], par.list[[i]])
    }
  }
  return(par.list)
}

# converts a named list to a OMLRunParList
convertListToOMLRunParList = function(x, ps = NULL, component = NULL) {
  assertList(x, names = "unique")
  assertCharacter(component, null.ok = TRUE, len = length(x))
  par.names = names(x)
  if (!is.null(ps)) {
    for (i in names(x)) {
      x[[i]] = paramToString(ps$pars[[i]], x[[i]])
    }
  }
  par.settings = setNames(vector("list", length(x)), par.names)
  for (i in seq_along(x)) {
    par.settings[[i]] = makeOMLRunParameter(
      name = par.names[i],
      value = x[[i]],
      component = ifelse(is.null(component), NA_character_, component[i])
    )
  }
  setClasses(par.settings, "OMLRunParList")
}

paramToString = function(par, x) {
  assertClass(par, "Param")
  type = par$type
  if (type %in% c("numeric", "integer", "logical", "character"))
    as.character(x)
  else if (type %in% c("numericvector", "integervector", "logicalvector", "charactervector"))
    collapse(x)
  else if (type == "discrete")
    discreteValueToName(par, x)
  else if (type == "discretevector")
    collapse(discreteValueToName(par, x))
  else if (type %in% c("function", "untyped"))
    rawToChar(serialize(x, connection = NULL, ascii = TRUE))
}

stringToParam = function(par, x) {
  assertClass(par, "Param")
  assertCharacter(x)
  type = par$type
  if (type %in% c("numeric", "integer", "logical", "character"))
    do.call(paste0("as.", type), list(x))
  else if (type %in% c("numericvector", "integervector", "logicalvector", "charactervector", "discretevector"))
    do.call(paste0("as.", stri_replace_all_fixed(type, "vector", "")), list(strsplit(x, ",")[[1L]]))
  else if (type == "discrete")
    discreteNameToValue(par, x)
  else if (type %in% c("function", "untyped"))
    unserialize(charToRaw(x))
}



#' @export
as.data.frame.OMLRunParList = function(x, ...) {
  as.data.frame(convertOMLRunParListToTable(x))
}

#' @export
as.data.table.OMLRunParList = function(x, ...) {
  convertOMLRunParListToTable(x)
}
