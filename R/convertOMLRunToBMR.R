# run = getOMLRun(536513)
# run.prob = getOMLRun(542887) # run.prob = runTaskMlr(getOMLTask(59), makeLearner("classif.rpart", predict.type = "prob"))
# bench = benchmark(makeLearner("classif.rpart"), iris.task, measures = list(mlr::timetrain, mlr::timepredict, mlr::timeboth))
# bench.prob = benchmark(makeLearner("classif.rpart", predict.type = "prob"), iris.task, measures = list(mlr::timetrain, mlr::timepredict, mlr::timeboth, mlr::acc))
# convertOMLRunToBMR(run, mlr::auc)
convertOMLRunToBMR = function(run, measures) {
  assertChoice(run$task.type, c("Supervised Classification", "Supervised Regression"))
  assertSubset(measures, choices = names(lookupMeasures()))
  learners = makeLearner(gsub("\\(.*", "", run$flow.name))
  task.id = paste0("OpenML-Task-", run$task.id)
  # FIXME: why is there a flow_id column and where can we find the measures per fold values
  evals = run$output.data$evaluations
  missing.meas = measures[measures%nin%unique(evals$name)]
  if (length(missing.meas) != 0) 
    stopf("Missing measures {'%s'} in the evaluations slot of the run.", collapse(missing.meas, "','"))
  runtime = evals$value[evals$name == "usercpu_time_millis"]
  
  # FIXME: try to do this without downloading
  task = getOMLTask(run$task.id)
  task = convertOMLTaskToMlr(task)
  nclasses = length(task$mlr.task$task.desc$class.levels)
  
  pred = run$predictions
  pred$fold = pred$fold + 1
  pred[,"repeat"] = pred[,"repeat"] + 1
  
  # try to get predict.type based on "confidence." columns if values are intergish
  pred.class = ifelse(run$task.type == "Supervised Classification", 
    "PredictionClassif", "PredictionRegr")
  conf.cols = grepl("confidence", colnames(pred))
  conf.cols.intergish = sapply(pred[,conf.cols], function(x) isTRUE(checkIntegerish(x)))
  if (all(conf.cols.intergish) & pred.class == "PredictionClassif") {
    predict.type = "prob"
  } else predict.type = "response"
  
  # get predictions based on predict.type
  if (predict.type == "prob" & pred.class == "PredictionClassif") {
    y = pred[,conf.cols]
    colnames(y) = gsub("confidence[.]", "", colnames(y))
  } else y = pred$prediction
  
  prediction = mlr:::makePrediction(task$mlr.task$task.desc, id = pred$row_id, 
    truth = pred$truth, y = y, row.names = pred$row_id,
    predict.type = predict.type, time = runtime)
  
  # FIXME: fix this for repeated CV and bootstrap
  # resamp.pred = makeS3Obj(c("ResamplePrediction", class(prediction)),
  #   instance = task$mlr.rin,
  #   predict.type = prediction$predict.type,
  #   data = cbind(prediction$data, iter = pred$fold, set = "test"),
  #   threshold = prediction$threshold,
  #   task.desc = prediction$task.desc,
  #   time = runtime
  # )
  # 
  # lapply(split(resamp.pred$data, as.factor(resamp.pred$data$iter)),
  #   function(x) performance(setClass(x, "Prediction"), mlr::mmce))
  # ms.test = vnapply(measures, function(pm) performance(pred = pred.test, measures = pm))
  
  ms.test = data.frame(iter = unique(pred$fold))
  ms.colnames = lookupMeasures()[[measures]]$id
  ms.test[,ms.colnames] = evals[evals$name%in%measures,][-1,"value"]
  results = list(
    learner.id = learners$id,
      task.id = task.id,
      measures.train = NA,#ms.train,
      measures.test = ms.test,#ms.test,
      aggr = setNames(mean(ms.test[,ms.colnames]), paste0(ms.colnames, ".test.mean")),
      pred = resamp.pred,
      models = NULL,
      err.msgs = data.frame(),
      extract = NULL,
      runtime = runtime
    )
  
  results = setNames(list(setNames(list(results), learners$id)), task.id)
  
  measures = lookupMeasures()[measures]
  makeS3Obj("BenchmarkResult",
    results = results,
    measures = measures,
    learners = setNames(list(learners), learners$id)
  )
}

# convertOMLPredictionsToMlrPredictions = function(predictions) {
#   
# }