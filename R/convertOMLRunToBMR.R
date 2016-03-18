#run = getOMLRun(536513)
# bench = benchmark(makeLearner("classif.rpart"), iris.task, measures = list(mlr::timetrain, mlr::timepredict, mlr::timeboth))
# convertOMLRunToBMR(run, mlr::auc)
convertOMLRunToBMR = function(run, measures) {
  assertChoice(run$task.type, c("Supervised Classification", "Supervised Regression"))
  learners = makeLearner(gsub("\\(.*", "", run$flow.name))
  task.id = paste0("OpenML-Task-", run$task.id)
  runtime = run$output.data$evaluations$value[run$output.data$evaluations$name=="usercpu_time_millis"]
  
  # FIXME: try to do this without downloading
  task = getOMLTask(run$task.id)
  task = convertOMLTaskToMlr(task)

  pred = run$predictions
  pred$fold = pred$fold + 1
  pred[,"repeat"] = pred[,"repeat"] + 1
  
  pred.class = ifelse(run$task.type == "Supervised Classification", "PredictionClassif", "PredictionRegr")
  # FIXME: allow predict.type prob (makePrediction makes trouble here)
  #predict.type = ifelse(pred.class == "PredictionClassif", "prob", "response")
  predict.type = "response"
  prediction = mlr:::makePrediction(task$mlr.task$task.desc, id = pred$row_id, 
    truth = pred$truth, y = pred$prediction, row.names = pred$row_id,
    predict.type = predict.type, time = runtime)
  
  # FIXME: fix this for repeated CV and bootstrap
  resamp.pred = makeS3Obj(c("ResamplePrediction", class(prediction)),
    instance = task$mlr.rin,
    predict.type = prediction$predict.type,
    data = cbind(prediction$data, iter = pred$fold, set = "test"),
    threshold = prediction$threshold,
    task.desc = prediction$task.desc,
    time = runtime
  )

  prediction = mlr:::makePrediction(task$mlr.task$task.desc, id = pred$row_id, 
    truth = pred$truth, y = pred$prediction, row.names = pred$row_id,
    predict.type = predict.type, time = runtime)
  
  # pred.data = namedList(c("id", "truth", "response", "prob"))
  # pred.data$id = pred$row_id
  # pred.data$truth = pred$truth
  # pred.data$response = pred$prediction
  # 
  # pred.data = as.data.frame(filterNull(pred.data))
  # ind = grepl("confidence", colnames(run$predictions))
  # if (any(ind)) {
  #   pred.data = cbind(pred.data, run$predictions[ind])
  #   colnames(pred.data) = gsub("confidence[.]", "prob.", colnames(pred.data))
  # }
  
  # p.ret =  makeS3Obj(c(pred.class, "Prediction"),
  #   predict.type = ifelse(pred.class == "PredictionClassif", "prob", "response"),
  #   data = pred.data,
  #   threshold = NA_real_,
  #   task.desc = NA,
  #   time = NA
  # )
  # 
  
  #lapply(split(resamp.pred$data, as.factor(resamp.pred$data$iter)),
  #  function(x) performance(setClass(x, "Prediction"), mlr::mmce))
  #ms.test = vnapply(measures, function(pm) performance(pred = pred.test, measures = pm))
  
  results = 
    list(
      learner.id = learners$id,
      task.id = task.id,
      measures.train = NA,#ms.train,
      measures.test = NA,#ms.test,
      aggr = performance(resamp.pred, mlr::mmce),#aggr,
      pred = resamp.pred,
      models = NULL,
      err.msgs = data.frame(),
      extract = NULL,
      runtime = runtime
    )
  
  addClasses(results, "BenchmarkResult")
  makeS3Obj("BenchmarkResult",
    results = results,
    measures = measures,
    learners = learners
  ) 
}

# convertOMLPredictionsToMlrPredictions = function(predictions) {
#   
# }