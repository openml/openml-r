#' Solve a task with MLR.
#'
#' Solve a task with a specified learner from MLR, compute predictions and upload the results 
#' to the OpenML server.
#' 
#' @param task [\code{\linkS4class{OpenMLTask}}]\cr 
#'   An OpenML task. Required.
#' @param learner [\code{\link[mlr]{Learner}}]\cr 
#'   Learner object from package mlr to solve the task.
#' @param return.mlr.results [\code{logical(1)}]\cr 
#'   Should not only the predictions but all of the by MLR computed information be returned? 
#'   This includes test measures in each step of the resampling procedure as well as the aggregated 
#'   performance. See \code{\link[mlr]{resample}}. Default is \code{TRUE}.
#' @param remove.const.features [\code{logical(1)}]\cr
#'   Should constant features be removed? Default is \code{TRUE}. Note, that setting this 
#'   to \code{FALSE} may lead to fatal errors. 
#' @param ... [any]\cr
#'   Further arguments that are passed to \code{\link[mlr]{removeConstantFeatures}}. 
#' @return List of:
#'   \item{run.pred}{[\code{\link[mlr]{ResamplePrediction}}]\cr
#'     Predictions resulting from the run. These are necessary in order to upload a run.}
#'   \item{mlr.resample.results}{[\code{list}]\cr
#'     The results of the MLR function \code{\link[mlr]{resample}}.\cr
#'     \code{NULL} if \code{return.mlr.results == FALSE}.}  
#' @seealso \code{\linkS4class{OpenMLTask}}, \code{\link[mlr]{learners}}, 
#'   \code{\link{authenticateUser}}, \code{\link[mlr]{resample}}
#' @export
# FIXME: if !return.mlr.results, the output is not a list!
runTask = function(task, learner, return.mlr.results = FALSE, remove.const.feats = TRUE, ...) {
  checkArg(task, "OpenMLTask")
  checkArg(learner, "Learner")
  checkArg(return.mlr.results, "logical")
  if((task$task.type == "Supervised Classification" && learner$type != "classif") ||
    (task$task.type == "Supervised Regression" && learner$type != "regr"))
    stopf("Learner type ('%s') does not correspond to task type ('%s').", task$task.type, learner$type)
  mlr.task = toMLR(task)
  
  if(remove.const.feats)
    mlr.task$mlr.task = removeConstantFeatures(x = mlr.task$mlr.task, ...)
  
  res = resample(learner, mlr.task$mlr.task, mlr.task$mlr.rin, measures = mlr.task$mlr.measures)
  pred = reformatPredictions(pred = res$pred$data, task = task)
  results = list(
    run.pred = pred, 
    mlr.resample.results = res
  )  
  if(!return.mlr.results) {
    results$mlr.resample.results = NULL
    results = results$run.pred
  }
  return(results)
}

# Reformat predictions
#
# Reformat an MLR predictions data.frame, so that it fits the OpenML expectencies.
#
# @param pred [\code{\link[mlr]{Prediction}}]\cr 
#   MLR predictions data.frame.
# @param task [\code{\linkS4class{OpenMLTask}}]\cr 
#   The OpenML task to which the predictions belong.
# @return data.frame with columns:
#   \item{repeat}{[\code{numeric}}]\cr
#     Current repetition of the estimation procedure.}
#   \item{fold}{[\code{numeric}]\cr
#     Current repetition of the estimation procedure.}  
#   \item{row_id}{[\code{numeric}]\cr
#     The observation's row ID.}
#   \item{prediction}{[\code{factor}]\cr
#     The predicted class.}  
#   \item{confidence."classname"}{[\code{numeric}]\cr
#     The predicted probability for class "classname". One column for each class. 
#     If no probabilities are provided, the predicted class gets probability 1 and each other class 
#     gets probability 0.}  

reformatPredictions = function(pred, task) {
  iter = pred$iter
  n = length(iter)
  folds = task$task.estimation.procedure$parameters$number_folds
  reps = task$task.estimation.procedure$parameters$number_repeats
  rep = rep(1:reps, each = n/reps)
  fold = iter %% folds
  fold[fold == 0] = folds
  rowid = pred$id
  
  # Note: The columns rep, fold and row_id must be 0-based to be accepted by the server.
  new_pred = data.frame(rep = rep - 1, fold = fold - 1, row_id = rowid - 1, prediction = pred$response)
  
  if(task$task.type == "Supervised Classification") {
    classes = levels(pred$response)
    probs = c()
    if(all(sprintf("prob.%s", classes) %in% colnames(pred))) {
      for(i in 1:length(classes)) {
        probs = cbind(probs, pred[, sprintf("prob.%s", classes[i])])
      }
    } else {
      for(i in 1:length(classes)) {
        probs = cbind(probs, ifelse(pred$response == classes[i], 1, 0))
      } 
    }
    colnames(probs) = sprintf("confidence.%s", classes) 
    
    new_pred = cbind(new_pred, probs)
  }
  
  colnames(new_pred)[1] = "repeat"  
  return(new_pred)
}