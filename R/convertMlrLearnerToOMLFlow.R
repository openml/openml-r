#' @title convertMlrLearnerToOMLFlow
#'
#' @description
#' Creates an OMLFlow for an mlr learner.
#' Required if you want to upload an mlr learner.
#'
#' @param lrn [\code{\link[mlr]{Learner}}]\cr
#'   The mlr learner.
#' @param name [\code{character(1)}]\cr
#'   The name of the flow object. Default is the learner ID.
#' @param description [\code{character(1)}]\cr
#'   An optional description of the learner.
#'   Default is a short specification of the learner and the associated package.
#' @param ... [\code{any}]\cr
#'   Further optional parameters that are passed to \code{\link{makeOMLFlow}}.
#' @return [\code{\link{OMLFlow}}].
convertMlrLearnerToOMLFlow = function(lrn, name = paste0("mlr.", lrn$id), description = NULL, ...) {
  # This function has been renamed, it was called createOMLFlowForMlrLearner
  assertClass(lrn, "Learner")
  assertString(name)
  
  #lrn = removeDefaultsFromParamValues(lrn)
  lrn = removeAllHyperPars(lrn)
  
  if (!is.null(description))
    assertString(description)
  else
    description = sprintf("Learner %s from package(s) %s.", name, collapse(lrn$package, sep = ", "))
  
  # dependencies
  pkges = getDependencies(lrn)
  
  # create sourcefile
  #sourcefile = createLearnerSourcefile(lrn)
  #external.version = paste0("R_", digest(file = sourcefile)) #digest(file = sourcefile)
  #on.exit(unlink(sourcefile))
  
  binary.path = file.path(tempdir(), sprintf("%s_binary.Rds", lrn$id))
  saveRDS(lrn, file = binary.path)
  
  # FIXME: use only hash when OpenML is on CRAN!
  external.version = paste0("R_", collapse(R.Version()[c("major", "minor")], "."), 
    "-v2.", digest(algo = "crc32", file = binary.path))
  
  flow = makeOMLFlow(
    name = name,
    description = description,
    parameters = makeFlowParameterList(lrn),
    dependencies = pkges,
    external.version = external.version,
    binary.path = binary.path,
    ...
  )
  
  # if (!is.null(lrn$next.learner)) {
  #   identifier = gsub(".*[.]", "", lrn$next.learner$id) #stri_split_fixed(lrn$next.learner$id, ".")[[1L]][2L]
  #   flow$components = list(convertMlrLearnerToOMLFlow(lrn$next.learner))
  #   names(flow$components) = identifier
  # }
  
  if (!is.null(lrn$next.learner)) {
    flow$components = lapply(getAllNextLearners(lrn), convertMlrLearnerToOMLFlow)
  }
  
  return(flow)
}

# extracts version dependencies including R, OpenML and mlr version
getDependencies = function(lrn) {
  lrn.package = ifelse(grepl("^!", lrn$package), gsub("^!", "", lrn$package), lrn$package)
  if ("mlr"%in%lrn.package) pkges = lrn.package else pkges = c("mlr", lrn.package)
  pkges = c("OpenML", pkges)
  pkges = sapply(pkges, function(x) sprintf("%s_%s", x, packageVersion(x)))
  pkges = c(paste0("R_", collapse(R.Version()[c("major", "minor")], ".")), pkges)
  pkges = collapse(pkges, sep = ", ")
  return(pkges)
}

# A list that extracts all next.learner recursively
getAllNextLearners = function(lrn) {
  getNextLearner = function(lrn, i) lrn$next.learner
  # get number of available next learners
  depth = stri_count_fixed(lrn$next.learner$id, ".")
  # extract next.learner recursively, 'depth' times
  all.next.learner = Reduce(getNextLearner, x = 1:depth, init = lrn$next.learner, accumulate = TRUE)
  return(setNames(all.next.learner, lapply(all.next.learner, function(x) gsub(".*[.]", "", x$id))))
}

removeAllHyperPars = function(mlr.lrn) {
  all.pars = names(getHyperPars(mlr.lrn))
  if (!is.null(all.pars)) mlr.lrn = removeHyperPars(mlr.lrn, ids = all.pars)
  
  if (!is.null(mlr.lrn$next.learner))
    mlr.lrn$next.learner = removeAllHyperPars(mlr.lrn$next.learner)
  
  return(mlr.lrn)
}

# removeDefaultsFromParamValues = function(mlr.lrn) {
#   par.defaults = getDefaults(getParamSet(mlr.lrn))
#   par.vals = mlr.lrn$par.vals
#   par.ind = vlapply(names(par.vals), function(x) !isTRUE(all.equal(par.defaults[[x]] , par.vals[[x]])))
#   mlr.lrn$par.vals = par.vals[par.ind]
#   
#   if (!is.null(mlr.lrn$next.learner))
#     mlr.lrn$next.learner = removeDefaultsFromParamValues(mlr.lrn$next.learner)
#   
#   return(mlr.lrn)
# }

# Generate a list of OpenML flow parameters for a given mlr learner.
#
# @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr
#   The mlr learner.
# @return A list of \code{\link{OpenMLFlowParameter}s}.
# @examples
# library(mlr)
# lrn = makeLearner("classif.randomForest")
# pars = makeFlowParameterList(lrn)
# pars
makeFlowParameterList = function(mlr.lrn) {
  pars = getParamSet(mlr.lrn)$pars #mlr.lrn$par.set$pars
  par.list = vector("list", length = length(pars))
  for(i in seq_along(pars)){
    name = pars[[i]]$id
    data.type = pars[[i]]$type
    # FIXME: data.type Should be either integer, numeric, string, vector, matrix, object.
    # if(data.type == "discrete") data.type = "string"      ?
    # if(data.type == "numericvector") data.type = "vector" ?
    # ...
    # For now, we don't want to store default values on the server.
    default.value = NA_character_
    flow.par = makeOMLFlowParameter(
      name = name,
      data.type = data.type,
      default.value = default.value
    )
    par.list[[i]] = flow.par
  }
  seed.pars = setNames(c(1, RNGkind()), c("openml.seed", "openml.kind", "openml.normal.kind"))
  par.list = append(par.list, lapply(seq_along(seed.pars), function(x) {
    makeOMLFlowParameter(
      name = names(seed.pars[x]),
      data.type = ifelse(is.numeric(seed.pars[x]), "integer", "discrete"),
      default.value = seed.pars[x]
    )}))
  return(par.list)
}
