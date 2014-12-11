#' @title Run mlr learner on OpenML task.
#'
#' Run task with a specified learner from mlr and produce predictions.
#'
#' @param task [\code{\link{OMLTask}}]\cr
#'   An OpenML task.
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   Learner from package mlr to run the task.
#' @param remove.const.feats [\code{logical(1)}]\cr
#'   Should constant features be removed?
#'   Default is \code{TRUE}.
#' @param ... [any]\cr
#'   Further arguments that are passed to \code{\link[mlr]{removeConstantFeatures}}.
#' @template arg_verbosity
#' @return [\code{OMLMlrRun}], an \code{\link{OMLRun}} with an additional slot \code{mlr.resample.result}.
#' @seealso \code{\link{getOMLTask}}, \code{\link[mlr]{makeLearner}}
#' @export
runTaskMlr = function(task, learner, remove.const.feats = TRUE, ..., verbosity = NULL) {

  assertClass(task, "OMLTask")
  assertClass(learner, "Learner")
  assertFlag(remove.const.feats)

  if ((task$type == "Supervised Classification" && learner$type != "classif") ||
    (task$type == "Supervised Regression" && learner$type != "regr") ||
    (task$type == "Survival Analysis" && learner$type != "surv") ||
    (task$type == "Clustering" && learner$type != "cluster")) {
    stopf("Learner type ('%s') does not correspond to task type ('%s').", task$type, learner$type)
  }

  run = makeOMLRun(task.id = task$id)
  mlr.task = toMlr(task)

  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  show.info = (verbosity > 0L)

  if (remove.const.feats)
    mlr.task$mlr.task = removeConstantFeatures(mlr.task$mlr.task, show.info = show.info, ...)

  res = try(resample(learner, mlr.task$mlr.task, mlr.task$mlr.rin, measures = mlr.task$mlr.measures,
    show.info = show.info), silent = TRUE)
  if (!is.error(res)) {
    run$predictions = reformatPredictions(pred = res$pred$data, task = task, orig.lvls = mlr.task$orig.lvls)
    run$mlr.resample.result = res
  } else {
    run$error.message = res[1]
  }
  class(run) = c("OMLMlrRun", "OMLRun")
  return(run)
}

# Reformat predictions
#
# Reformat an mlr predictions data.frame, so that it fits the OpenML expectencies.
#
# @param pred [\code{\link[mlr]{Prediction}}]\cr
#   The predictions.
# @param task [\code{\link{OMLTask}}]\cr
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

reformatPredictions = function(pred, task, orig.lvls) {
  iter = pred$iter
  n = length(iter)
  folds = task$estimation.procedure$parameters$number_folds
  reps = task$estimation.procedure$parameters$number_repeats
  rep = rep(seq_len(reps), each = n/reps)
  fold = iter %% folds
  fold[fold == 0L] = folds
  rowid = pred$id

  # Note: The columns rep, fold and row_id must be 0-based to be accepted by the server.
  new.pred = data.frame(rep = rep - 1L, fold = fold - 1L, row_id = rowid - 1L, prediction = pred$response)

  if (task$type == "Supervised Classification") {
    probs = c()
    for (lvl in orig.lvls) {
      if (sprintf("prob.%s", lvl) %in% colnames(pred))
        probs = cbind(probs, pred[, sprintf("prob.%s", lvl)])
      else probs = cbind(probs, ifelse(pred$response == lvl, 1, 0))
    }
    colnames(probs) = sprintf("confidence.%s", orig.lvls)

    new.pred = cbind(new.pred, probs)
  }

  new.pred$prediction = factor(as.character(new.pred$prediction), levels = orig.lvls)
  colnames(new.pred)[1L] = "repeat"
  return(new.pred)
}