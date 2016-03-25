#' Reproduce the Run
#'
#' Uses the ID of the run and tries to reproduce its results by downloading the flow and appling it to the respecitve task.
#'
#' @param task [\code{\link{OMLTask}}]\cr
#'   Task
#' @param flow [\code{\link{OMLFlow}}]\cr
#'   Flow that is applied to the Task
#' @param par.vals [\code{list}]\cr
#'   List of Hyperparameters
#' @template arg_verbosity
#' @return [\code{OMLMlrRun}], an \code{\link{OMLRun}}.
# @export
# @family run related functions
runTaskFlow = function(task, flow, par.vals, verbosity = NULL) {
  #run = getOMLRun(run.id)
  if (grepl("-v1[[:punct:]]", flow$external.version)) {
    seed.pars = c("openml.seed", "openml.kind", "openml.normal.kind")
    kind.var = c("openml.kind", "openml.normal.kind")
  } else if (grepl("R_", flow$external.version)) {
    seed.pars = c("seed", "kind", "normal.kind")
    kind.var = c("kind", "normal.kind")
  } else {
    stop("This flow can't be run in R.")
  }
  # FIXME: converter par.vals to OMLRunParList and vice versa
  #par.vals = lapply(run$parameter.setting, function(x) x$value)
  #names(par.vals) = vcapply(run$parameter.setting, function(x) x$name)
  
  # get task and flow
  #task = getOMLTask(run$task.id)
  #flow = getOMLFlow(run$flow.id)
  
  # FIXME: check flow name
  # make learner with parameters
  lrn = makeLearner(flow$name)
  # assign data type to learner parameters 
  lrn.pars = par.vals[!names(par.vals)%in%seed.pars]
  lrn.pars.type = vcapply(lrn$par.set$pars, function(x) x$type)[names(lrn.pars)]
  for (i in seq_along(lrn.pars)) {
    if (lrn.pars.type[i] == "integer") lrn.pars[[i]] = as.integer(lrn.pars[[i]])
    if (lrn.pars.type[i] == "numeric") lrn.pars[[i]] = as.numeric(lrn.pars[[i]])
    if (lrn.pars.type[i] == "discrete") lrn.pars[[i]] = as.character(lrn.pars[[i]])
  }
  lrn = do.call("setHyperPars", append(list(learner = lrn), list(par.vals = lrn.pars)))
  
  # FIXME: warn if installed package version are not equal
  local.pkges = vcapply(c("mlr", lrn$package), function(x) sprintf("%s_%s", x, packageVersion(x)))
  flow.pkges = strsplit(flow$dependencies, ", ")[[1]]
  local.diff = setdiff(local.pkges, flow.pkges)
  if (length(local.diff) != 0) 
    messagef("Flow has been created with %s, but you have installed %s.", 
      collapse(setdiff(flow.pkges, local.pkges), ", "), collapse(local.diff, ", "))
  
  # set seed info
  currentRNG = RNGkind()
  do.call("RNGkind", par.vals[kind.var])
  if (!identical(currentRNG, RNGkind())) 
    messagef("Current RNG kind has been changed.")
  seed = par.vals$seed
  # FIXME: set default seed info
  
  # execute setup.string
  ret = runTaskMlr(task = task, learner = lrn, verbosity = verbosity, seed = seed) #eval(parse(text=run$setup.string))
  #ret$run.id = run$run.id
  
  return(ret)
}
