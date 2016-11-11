#' @title Converts an OMLFlow to an mlr learner.
#'
#' @description
#' Creates an \code{\link{OMLFlow}} for an \pkg{mlr} \code{\link[mlr]{Learner}}]
#' Required if you want to upload an mlr learner to the OpenML server.
#'
#' @param lrn [\code{\link[mlr]{Learner}}]\cr
#'   The mlr learner.
#' @param name [\code{character(1)}]\cr
#'   The name of the flow object. Default is the learner ID with the prefix
#'   \dQuote{mlr} prepended.
#' @param description [\code{character(1)}]\cr
#'   An optional description of the learner.
#'   Default is a short specification of the learner and the associated package.
#' @param ... [\code{any}]\cr
#'   Further optional parameters that are passed to \code{\link{makeOMLFlow}}.
#' @return [\code{\link{OMLFlow}}].
#' @export
convertMlrLearnerToOMLFlow = function(lrn, name = paste0("mlr.", lrn$id), description = NULL, ...) {
  # This function has been renamed, it was called createOMLFlowForMlrLearner
  assertClass(lrn, "Learner")
  assertString(name)

  #lrn = removeDefaultsFromParamValues(lrn)
  lrn = removeAllHyperPars(lrn)

  if (is.null(description))
    description = sprintf("Learner %s from package(s) %s.", name, collapse(lrn$package, sep = ", "))
  assertString(description)

  # get R/mlr version information
  dependencies = getDependencies(lrn)

  # create sourcefile
  #sourcefile = createLearnerSourcefile(lrn)
  #external.version = paste0("R_", digest(file = sourcefile)) #digest(file = sourcefile)
  #on.exit(unlink(sourcefile))

  # save learner object to RDS file
  binary.path = file.path(tempdir(), sprintf("%s_binary.Rds", lrn$id))
  saveRDS(lrn, file = binary.path)

  # FIXME: use only hash when OpenML is on CRAN!
  external.version = paste0(getRVersionString(), "-v2.", digest(algo = "crc32", file = binary.path))

  flow = makeOMLFlow(
    name = name,
    description = description,
    parameters = makeFlowParameterList(lrn),
    dependencies = dependencies,
    external.version = external.version,
    binary.path = binary.path,
    object = lrn,
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

# @title Get learner dependencies.
#
# @description
# Extracts version dependencies including R, OpenML and mlr version.
#
# @param lrn [Learner]
#   Learner from mlr package.
# @return [character(1)]
getDependencies = function(lrn) {
  # remove starting !
  lrn.package = ifelse(grepl("^!", lrn$package), gsub("^!", "", lrn$package), lrn$package)
  # check if mlr is needed
  dependencies = if ("mlr" %in% lrn.package) lrn.package else c("mlr", lrn.package)
  # OpenML is always needed
  dependencies = c("OpenML", dependencies)
  # append package version to each package
  dependencies = vcapply(dependencies, function(x) sprintf("%s_%s", x, packageVersion(x)))
  # finally add "R version string"
  dependencies = c(getRVersionString(), dependencies)

  dependencies = collapse(dependencies, sep = ", ")
  return(dependencies)
}

# @title Extract wrapped learners recursively.
#
# @param lrn [Learner]
#   Learner from mlr package.
# @return [list] Named list of learners.
getAllNextLearners = function(lrn) {
  getNextLearner = function(lrn, i) lrn$next.learner
  # get number of available next learners
  depth = stri_count_fixed(lrn$next.learner$id, ".")
  # extract next.learner recursively, 'depth' times
  all.next.learner = Reduce(getNextLearner, x = 1:depth, init = lrn$next.learner, accumulate = TRUE)
  return(setNames(all.next.learner, lapply(all.next.learner, function(x) gsub(".*[.]", "", x$id))))
}

# @title Recursively remove learner parameters.
#
# @param lrn [Learner]
#   Learner from mlr package.
# @return [Learner]
removeAllHyperPars = function(lrn) {
  all.pars = names(mlr::getHyperPars(lrn))
  if (!is.null(all.pars))
    lrn = mlr::removeHyperPars(lrn, ids = all.pars)

  # proceed recursively
  if (!is.null(lrn$next.learner))
    lrn$next.learner = removeAllHyperPars(lrn$next.learner)

  return(lrn)
}

# @title Generate a list of OpenML flow parameters for a given mlr learner.
#
# @param lrn [\code{\link[mlr]{Learner}}]\cr
#   The mlr learner.
# @return A list of \code{\link{OpenMLFlowParameter}s}.
# @examples
# library(mlr)
# lrn = makeLearner("classif.randomForest")
# pars = makeFlowParameterList(lrn)
# pars
makeFlowParameterList = function(lrn) {
  par.list = makeFlowParameterListForMlrLearner(lrn)
  par.list = append(par.list, makeFlowParameterListFor())
  return(par.list)
}

# @title Helper to create parameters for mlr learner.
#
# @param lrn [Learner]
#   Learner from package mlr.
# @return [list] of OMLFlowParameter objects.
makeFlowParameterListForMlrLearner = function(lrn) {
  # create list of OpenMLFlowParameters
  lapply(getParamSet(lrn)$pars, function(par) {
    makeOMLFlowParameter(
      name = par$id,
      data.type = par$type,
      default.value = NA_character_  # For now, we don't want to store default values on the server.
    )
  })
}

# @title Helper to create parameters for random numbers generator.
#
# @return [list] of OMLFlowParameter objects.
makeFlowParameterListFor = function() {
  # now handle random numbers generator seeding
  seed.pars = setNames(c(1, RNGkind()), c("openml.seed", "openml.kind", "openml.normal.kind"))
  lapply(seq_along(seed.pars), function(x) {
    makeOMLFlowParameter(
      name = names(seed.pars[x]),
      data.type = ifelse(is.numeric(seed.pars[x]), "integer", "discrete"),
      default.value = seed.pars[x]
  )})
}

# removeDefaultsFromParamValues = function(lrn) {
#   par.defaults = getDefaults(getParamSet(lrn))
#   par.vals = lrn$par.vals
#   par.ind = vlapply(names(par.vals), function(x) !isTRUE(all.equal(par.defaults[[x]] , par.vals[[x]])))
#   lrn$par.vals = par.vals[par.ind]
#
#   if (!is.null(lrn$next.learner))
#     lrn$next.learner = removeDefaultsFromParamValues(lrn$next.learner)
#
#   return(lrn)
# }
