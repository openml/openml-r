context("check arff files")

test_that("check arff files", {
  lrn = makeLearner("classif.rpart")
  task = getOMLTask(59)
  res = runTaskMlr(task, lrn)
  
  output = tempfile()
  setOMLConfig(arff.reader = "farff")
  arff.writer(res$run$predictions, file = output)
  run.arff = arff.reader(output)
  
  output = tempfile()
  setOMLConfig(arff.reader = "RWeka")
  arff.writer(res$run$predictions, file = output)
  run.arff = arff.reader(output)
})
