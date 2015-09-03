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

reformatPredictions = function(pred, task) {
  iter = pred$iter
  n = length(iter)
  folds = task$input$estimation.procedure$parameters$number_folds
  reps = task$input$estimation.procedure$parameters$number_repeats
  rep = rep(seq_len(reps), each = n/reps)
  fold = iter %% folds
  fold[fold == 0L] = folds
  rowid = pred$id
  if (task$task.type == "Supervised Classification")
    orig.lvls = levels(task$input$data.set$data[, task$input$target.features])
  else
    orig.lvls = NULL

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

