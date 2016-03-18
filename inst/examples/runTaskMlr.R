\dontrun{
  library(mlr)
  ## run a single flow (learner) on a single task
  task = getOMLTask(57)
  lrn = makeLearner("classif.rpart")
  res = runTaskMlr(task, lrn)
  ## the result "res" is a list, storing information on the actual "run", the
  ## corresponding benchmark result "bmr" and the applied "flow"
}
