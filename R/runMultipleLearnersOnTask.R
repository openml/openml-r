#' @title Run multiple mlr learners on OpenML task.
#'
#' Run task with a specified learners from mlr and produce predictions.
#'
#' @param task [\code{\link{OMLTask}}]\cr
#'   An OpenML task.
#' @param learner [list of \code{\link[mlr]{Learner}}]\cr
#'   Learner from package mlr to run the task.
#' @param remove.const.feats [\code{logical(1)}]\cr
#'   Should constant features be removed?
#'   Default is \code{TRUE}.
#' @template arg_verbosity
#' @param auto.upload [\code{logical(1)}]\cr
#'   Checks whether an \code{\link{OMLFlow}} object containing the passed \code{learner} 
#'   was already uploaded to the server. 
#'   
#'   If it has not been found on the server and \code{auto.upload = TRUE}, 
#'   a new \code{implementation.id} is assigned and the \code{\link{OMLFlow}} is 
#'   automatically uploaded. If the \code{learner} was already uploaded, the 
#'   \code{implementation.id} of the respective \code{\link{OMLFlow}} is used.
#'   
#'   If \code{auto.upload = FALSE}, only the \code{implementation.id} of an 
#'   already uploaded \code{learner} is used and an error is returned if the 
#'   \code{learner} was not found on the server.
#' @param ... [any]\cr
#'   Further arguments that are passed to \code{\link[mlr]{removeConstantFeatures}}.
#' @return [\code{OMLMlrRun}], an \code{\link{OMLRun}} with an additional slot \code{mlr.resample.result}.
#' @seealso \code{\link{getOMLTask}}, \code{\link[mlr]{makeLearner}}
#' @export
#' 
runMultipleLearnersOnTask = function(task, learner, remove.const.feats = TRUE, 
                      verbosity = NULL, auto.upload = TRUE, ...) {
  # FIXME: This function should be merged with runTaskMlr
  assertClass(task, "OMLTask")
  assertFlag(remove.const.feats)
  
  run = makeOMLRun(task.id = task$task.id)
  
  mlr.task = toMlr(task, verbosity = verbosity)
  
  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  show.info = (verbosity > 0L)
  
  if (remove.const.feats)
    mlr.task$mlr.task = removeConstantFeatures(mlr.task$mlr.task, show.info = show.info, ...)
  
  if (inherits(learner, "list")) {
    bench = try(benchmark(learner, mlr.task$mlr.task, mlr.task$mlr.rin, measures = mlr.task$mlr.measures,
                          show.info = show.info), silent = TRUE)
    res = bench$results$data
  } else {
    stop("learner must be a list")
  }
  
  runs = check = vector("list", length = length(learner))
  names(runs) = names(res)
  for(i in 1:length(learner)) {
    runs[[i]] = run

    runs[[i]]$predictions = reformatPredictions(pred = res[[i]]$pred$data, task = task, orig.lvls = mlr.task$orig.lvls)
    runs[[i]]$mlr.resample.result = res[[i]]

    class(runs[[i]]) = c("OMLMlrRun", "OMLRun")
    
    runs[[i]]$parameter.setting = makeOMLRunParList(learner[[i]])
    check[[i]] = checkOMLFlow(learner[[i]], verbosity = verbosity)
    
    if(check[[i]]$exists) {
      runs[[i]]$implementation.id = xmlOValI(check[[i]]$doc, "/oml:implementation_exists/oml:id")
    } else {
      if(auto.upload) {
        runs[[i]]$implementation.id = uploadOMLFlow(learner[[i]], verbosity = verbosity)
      } else {
        stopf("Flow does not exist, use 'auto.upload = TRUE' to upload it.")
      }
    }
  }
  return(list(runs = runs, benchmark = bench))
}