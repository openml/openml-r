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
  if (!is.null(prefix))
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
print.OMLSeedParList = function(x, ...)  {
  #x = unclass(x)
  catf("This is a '%s' with the following parameters:", class(x)[1])
  if (length(x) > 0)
    x = rbindlist(lapply(x, function(x) x[c("name", "value", "component")])) else
      x = data.frame()
  x$component = NULL
  print(x)
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
  #assertList(par[isSeedPar(par)], len = 3)
  return(setClasses(par[isSeedPar(par)], "OMLSeedParList"))
}


# hepler functions:
isSeedPar = function(par) {
  rpl.names = vcapply(par, function(x) x$name)
  stri_detect_regex(rpl.names, "(seed$|kind$|normal.kind$)")
}

# @param x OMLSeedParList
setOMLSeedParList = function(x, flow = NULL) {
  assertClass(x, "OMLSeedParList")
  seed.pars = vcapply(x, function(x) x$value)
  prefix = unique(gsub("seed|kind|normal.kind", "", names(seed.pars)))
  names(seed.pars) = gsub(prefix, "", names(seed.pars)) #c("seed", "kind", "normal.kind")
  xRNG = seed.pars[c("kind", "normal.kind")]

  currentRNG = RNGkind()
  if (!identical(currentRNG, unname(xRNG)))
    messagef("Kind of RNG has been changed to '%s'",
      convertToShortString(as.list(xRNG)))

  if (!is.null(flow)) RNGversion(extractRVersionFromFlow(flow))

  do.call("set.seed", as.list(seed.pars))
}

# extracts the seed value of a OMLSeedParList
extractSeed = function(x) {
  assertClass(x, "OMLSeedParList")
  seed.names = vcapply(x, function(x) x$name)
  seed = vcapply(x, function(x) x$value)[stri_detect_fixed(seed.names, "seed")]
  as.integer(seed)
}
