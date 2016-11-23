#' @title Convert an OpenML run set to a benchmark result for mlr.
#'
#' @description
#' Converts an \code{\link{OMLRun}} to a \code{\link[mlr]{BenchmarkResult}}.
#'
#' @param run [\code{\link{OMLRun}}]\cr
#'   The run that should be converted.
#' @param measures [\code{character}]\cr
#'   Character describing the measures (see \code{\link{listOMLEvaluationMeasures}})
#'   that will be converted into mlr \code{\link[mlr]{measures}} and are then used in the \code{\link[mlr]{BenchmarkResult}}.
#'   Currently, not all measures from OpenML can be converted into mlr measures.
#' @param recompute [\code{logical(1)}]\cr
#'   Should the measures be recomputed with mlr using the predictions? Currently recomputing is not supported.
#' @return [\code{\link[mlr]{BenchmarkResult}}].
#' @family run-related functions
#' @export
convertOMLRunToBMR = function(run, measures, recompute = FALSE) {
  assertChoice(run$task.type, c("Supervised Classification", "Supervised Regression"))
  assertSubset(measures, choices = names(lookupMeasures()))
  # FIXME: allow that measures are recomputed with mlr using the predictions
  assertFlag(recompute)

  # FIXME: try to do this without downloading, if it is possible?
  task = getOMLTask(run$task.id)
  flow = getOMLFlow(run$flow.id)
  flow.version = getFlowExternalVersion(flow)

  if (flow.version >= 2) {
    learners = readRDS(flow$binary.path)
  } else {
    learners = mlr::makeLearner(gsub("\\(.*", "", run$flow.name))
  }

  task.id = paste(task$input$data.set$desc$name, "task", task$task.id, sep = ".") #paste0("OpenML-Task-", run$task.id)
  # FIXME: why is there a flow_id column and where can we find the measures per fold values
  evals = run$output.data$evaluations
  missing.meas = measures[measures%nin%unique(evals$name)]
  if (length(missing.meas) != 0)
    stopf("You requested the measures {'%s'}. However, only {'%s'} are available in the evaluations slot of the run.",
      collapse(missing.meas, "', '"),
      collapse(unique(evals$name), "', '"))
  runtime = evals$value[evals$name == "usercpu_time_millis"]

  task = convertOMLTaskToMlr(task)

  pred = run$predictions
  if(min(pred$fold) == 0)
    pred$fold = (pred$fold + 1)
  if(min(pred[,"repeat"]) == 0)
    pred[,"repeat"] = (pred[,"repeat"] + 1)

  # try to get predict.type based on "confidence." columns if values are intergish
  pred.class = ifelse(run$task.type == "Supervised Classification",
    "PredictionClassif", "PredictionRegr")
  conf.cols = stri_detect_fixed(colnames(pred), "confidence")
  conf.cols.intergish = vlapply(pred[, conf.cols, drop = FALSE], testIntegerish)
  if (all(!conf.cols.intergish) & pred.class == "PredictionClassif") {
    predict.type = "prob"
  } else predict.type = "response"

  pred.split = split(pred, as.factor(paste0(pred[,"repeat"], "-", pred$fold)))
  prediction = lapply(pred.split, function (pred) {
    # get predictions based on predict.type
    if (predict.type == "prob" & pred.class == "PredictionClassif") {
      y = pred[,conf.cols]
      colnames(y) = stri_replace_all_fixed(colnames(y), "confidence.", "")
    } else y = pred$prediction

    makeMlrPrediction(task$mlr.task$task.desc, id = pred$row_id,
      truth = pred$truth, y = y, row.names = pred$row_id,
      predict.type = predict.type, time = runtime)
  })
  pred.data = lapply(prediction, function(x) x$data)
  pred.data = pred.data[order(as.numeric(gsub(".*-","",names(pred.data))))]
  for(i in seq_along(pred.data)) pred.data[[i]]$iter = i
  pred.data = data.frame(rbindlist(pred.data), set = "test")

  threshold = unique(lapply(prediction, function(x) x$threshold))
  if(length(threshold) > 1)
    stopf("threshold must be a list of length 1")

  # FIXME: fix this for repeated CV and bootstrap
  resamp.pred = makeS3Obj(c("ResamplePrediction", pred.class, "Prediction"),
    instance = task$mlr.rin,
    predict.type = predict.type,
    data = pred.data, #cbind(prediction$data, iter = pred$fold, set = "test"),
    threshold = threshold,
    task.desc = task$mlr.task$task.desc,
    time = runtime
  )

  # lapply(split(resamp.pred$data, as.factor(resamp.pred$data$iter)),
  #   function(x) performance(setClass(x, "Prediction"), mlr::mmce))
  # ms.test = vnapply(measures, function(pm) performance(pred = pred.test, measures = pm))

  if (!recompute) {
    aggr.eval = evals[is.na(evals$fold) & is.na(evals[,"repeat"]), ]
    iter.eval = evals[!is.na(evals$fold) & !is.na(evals[,"repeat"]), ]
    iter.eval$iter = as.factor(paste0(iter.eval[,"repeat"], "-", iter.eval$fold))
    iter.eval.split = split(iter.eval, iter.eval$iter)
    getMeasureValue = function(eval, measures, as.df = TRUE) {
      eval = eval[eval$name %in% measures, ]#subset(eval, name %in% measures)
      ret = setNames(eval$value, eval$name)
      if (as.df) as.data.frame(t(ret)) else ret
    }
    iter.eval.split = rbindlist(lapply(iter.eval.split, getMeasureValue, measures = measures))
    colnames(iter.eval.split) = unname(vcapply(convertOMLMeasuresToMlr(colnames(iter.eval.split)), function(x) x$id))
    ms.test = data.frame(iter = seq_row(iter.eval.split), iter.eval.split)

    #ms.train = subset(ms.test, select = -iter)
    #ms.train[!is.na(ms.train)] = NA
    #ms.train = data.frame(iter = ms.test$iter, as.data.frame(ms.train))

    aggr = getMeasureValue(aggr.eval, measures = measures, as.df = FALSE)
    names(aggr) = unname(vcapply(convertOMLMeasuresToMlr(names(aggr)), function(x) x$id))
  } else {
    # FIXME: this is incomplete
  #   ms.test.df = lapply(prediction, function(x) mlr::performance(x, lookupMeasures()[measures]) )
  #   ms.test.df = do.call("rbind", ms.test.df)
  #   ms.test.df = data.frame(iter = row.names(ms.test.df), setRowNames(ms.test.df, NULL))
  #   ms.test.df$iter = as.integer(gsub(".*-", "", ms.test.df$iter))
  #   ms.test.df = ms.test.df[order(ms.test.df$iter),]
  #   aggr = rowMeans(as.data.frame(lapply(prediction, function(x) mlr::performance(x, lookupMeasures()[measures]) )))
  }

  results = list(
    learner.id = learners$id,
      task.id = task.id,
      measures.train = data.frame(),
      measures.test = ms.test,
      aggr = setNames(aggr, paste0(names(aggr), ".test.mean")),
      pred = resamp.pred,
      models = list(),
      err.msgs = data.frame(),
      extract = list(),
      runtime = runtime,
      learner = learners
    )

  results = setNames(list(setNames(list(results), learners$id)), task.id)

  measures = lookupMeasures()[measures]
  makeS3Obj("BenchmarkResult",
    results = results,
    measures = measures,
    learners = setNames(list(learners), learners$id)
  )
}

# FIXME: use mlr's makePrediction when version 2.10 is on CRAN
makeMlrPrediction = function(task.desc, row.names, id, truth, predict.type, 
  predict.threshold = NULL, y, time, error = NA_character_) {
  UseMethod("makeMlrPrediction")
}

makeMlrPrediction.TaskDescRegr = function(task.desc, row.names, id, truth, 
  predict.type, predict.threshold = NULL, y, time, error = NA_character_) {
  data = namedList(c("id", "truth", "response", "se"))
  data$id = id
  data$truth = truth
  if (predict.type == "response") {
    data$response = y
  } else {
    data$response = y[, 1L]
    data$se = y[, 2L]
  }
  makeS3Obj(c("PredictionRegr", "Prediction"),
    predict.type = predict.type,
    data = setRowNames(as.data.frame(filterNull(data)), row.names),
    threshold = NA_real_,
    task.desc = task.desc,
    time = time,
    error = error
  )
}

makeMlrPrediction.TaskDescClassif = function(task.desc, row.names, id, truth, 
  predict.type, predict.threshold = NULL, y, time, error = NA_character_) {
  data = namedList(c("id", "truth", "response", "prob"))
  data$id = id
  # truth can come from a simple "newdata" df. then there might not be all factor levels present
  if (!is.null(truth))
    levels(truth) = union(levels(truth), task.desc$class.levels)
  data$truth = truth
  if (predict.type == "response") {
    data$response = y
    data = as.data.frame(filterNull(data))
  } else {
    data$prob = y
    data = as.data.frame(filterNull(data))
    # fix columnnames for prob if strange chars are in factor levels
    indices = stri_detect_fixed(names(data), "prob.")
    if (sum(indices) > 0)
      names(data)[indices] = stri_paste("prob.", colnames(y))
  }
  p = makeS3Obj(c("PredictionClassif", "Prediction"),
    predict.type = predict.type,
    data = setRowNames(data, row.names),
    threshold = NA_real_,
    task.desc = task.desc,
    time = time,
    error = error
  )
  if (predict.type == "prob") {
    # set default threshold to 1/k
    if (is.null(predict.threshold)) {
      predict.threshold = rep(1/length(task.desc$class.levels), length(task.desc$class.levels))
      names(predict.threshold) = task.desc$class.levels
    }
    p = setThreshold(p, predict.threshold)
  }
  return(p)
}

# run = getOMLRun(536513)
# run.prob = getOMLRun(542887) # run.prob = runTaskMlr(getOMLTask(59), makeLearner("classif.rpart", predict.type = "prob"))
# bench = benchmark(makeLearner("classif.rpart"), iris.task, measures = list(mlr::timetrain, mlr::timepredict, mlr::timeboth))
# bench = benchmark(makeLearner("classif.rpart"), iris.task, measures = list(mlr::timeboth, mlr::timeboth), resampling = makeResampleDesc("RepCV", reps = 3, folds = 5))
# bench.prob = benchmark(makeLearner("classif.rpart", predict.type = "prob"), iris.task, measures = list(mlr::timetrain, mlr::timepredict, mlr::timeboth, mlr::acc))
# convertOMLRunToBMR(run, mlr::auc)

# convertOMLPredictionsToMlrPredictions = function(predictions) {
#
# }
