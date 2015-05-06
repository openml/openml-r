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
#' @param resample.extract [\code{function}]\cr
#'   Function used to extract information from a fitted model during the resampling done by \code{mlr}.
#'   Is applied to every \code{\link{WrappedModel}} resulting from calls to \code{\link{train}}
#'   during resampling.
#'   Default is to extract nothing.
#' @param ... [any]\cr
#'   Further arguments that are passed to \code{\link[mlr]{removeConstantFeatures}}.
#' @return [\code{OMLMlrRun}], an \code{\link{OMLRun}} with an additional slot \code{mlr.resample.result}.
#' @seealso \code{\link{getOMLTask}}, \code{\link[mlr]{makeLearner}}
#' @export
runTaskMlr = function(task, learner, remove.const.feats = TRUE, 
  verbosity = NULL, auto.upload = TRUE, resample.extract, ...) {

  assertClass(task, "OMLTask")
  assertClass(learner, "Learner")
  assertFlag(remove.const.feats)

  if ((task$task.type == "Supervised Classification" && learner$type != "classif") ||
    (task$task.type == "Supervised Regression" && learner$type != "regr") ||
    (task$task.type == "Survival Analysis" && learner$type != "surv") ||
    (task$task.type == "Clustering" && learner$type != "cluster")) {
    stopf("Learner type ('%s') does not correspond to task type ('%s').", task$task.type, learner$type)
  }

  run = makeOMLRun(task.id = task$task.id)
  mlr.task = toMlr(task, verbosity = verbosity)

  if (is.null(verbosity))
    verbosity = getOMLConfig()$verbosity
  show.info = (verbosity > 0L)

  if (remove.const.feats)
    mlr.task$mlr.task = removeConstantFeatures(mlr.task$mlr.task, show.info = show.info, ...)

  res = try(resample(learner, mlr.task$mlr.task, mlr.task$mlr.rin, measures = mlr.task$mlr.measures,
    extract = resample.extract, show.info = show.info), silent = TRUE)
  if (!is.error(res)) {
    run$predictions = reformatPredictions(pred = res$pred$data, task = task, orig.lvls = mlr.task$orig.lvls)
    run$mlr.resample.result = res
  } else {
    run$error.message = res[1]
  }
  class(run) = c("OMLMlrRun", "OMLRun")

  run$parameter.setting = makeOMLRunParList(learner)
  
  check = checkOMLFlow(learner, verbosity = verbosity)
  
  if(check$exists) {
    run$implementation.id = xmlOValI(check$doc, "/oml:implementation_exists/oml:id")
  } else {
    if(auto.upload) {
      run$implementation.id = uploadOMLFlow(learner, verbosity = verbosity)
    } else {
      stopf("Flow does not exist, use 'auto.upload = TRUE' to upload it.")
    }
  }
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
  folds = task$input$estimation.procedure$parameters$number_folds
  reps = task$input$estimation.procedure$parameters$number_repeats
  rep = rep(seq_len(reps), each = n/reps)
  fold = iter %% folds
  fold[fold == 0L] = folds
  rowid = pred$id

  # Note: The columns rep, fold and row_id must be 0-based to be accepted by the server.
  new.pred = data.frame(
    rep = rep - 1L,
    fold = fold - 1L,
    row_id = rowid - 1L,
    prediction = pred$response
  )

  if (task$task.type != "Survival Analysis") {
    new.pred$truth = pred$truth
  }

  if (task$task.type == "Supervised Classification") {
    probs = c()
    for (lvl in orig.lvls) {
      if (sprintf("prob.%s", lvl) %in% colnames(pred))
        probs = cbind(probs, pred[, sprintf("prob.%s", lvl)])
      else probs = cbind(probs, ifelse(pred$response == lvl, 1, 0))
    }
    colnames(probs) = sprintf("confidence.%s", orig.lvls)
    new.pred = cbind(new.pred, probs)

    new.pred$prediction = factor(as.character(new.pred$prediction), levels = orig.lvls)
    new.pred$truth = factor(as.character(new.pred$truth), levels = orig.lvls)
  }

  colnames(new.pred)[1L] = "repeat"
  return(new.pred)
}

# Generate a list of OpenML run parameter settings for a given mlr learner.
#
# @param mlr.lrn [\code{\link[mlr]{Learner}}]\cr
#   The mlr learner.
# @param component [\code{character}]\cr
#   If the learner is a (sub-)component of an implementation, this component's name.
# @return A list of \code{\link{OpenMLRunParameter}s}.
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
makeOMLRunParList = function(mlr.lrn, component = NA_character_) {
  assertClass(mlr.lrn, "Learner")
  assertString(component, na.ok = TRUE)
  
  par.vals = mlr.lrn$par.vals
  par.names = names(mlr.lrn$par.vals)
  par.settings = vector("list", length(par.vals))
  for (i in seq_along(par.vals)) {
    par.settings[[i]] = makeOMLRunParameter(
      name = par.names[i],
      value = as.character(par.vals[[i]]),
      component = component)
  }
  if (!is.null(mlr.lrn$next.learner)) {
    # Use the learner's id (without "classif." or "regr.") as the subcomponent's name...
    # FIXME: check if or make sure that this is correct
    component = strsplit(mlr.lrn$next.learner$id, split = ".", fixed = TRUE)[[1]][2]
    inner.par.settings = makeOMLRunParList(mlr.lrn$next.learner, component = component)
    par.settings = c(par.settings, inner.par.settings)
  }
  return(par.settings)
}