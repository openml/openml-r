library(mlr)
## run a single flow (learner) on a single task
conf = getOMLConfig()
print(conf$apikey)
setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")
conf = getOMLConfig()
print(conf$apikey)

task = getOMLTask(57)
lrn = makeLearner("classif.rpart")
res = runTaskMlr(task, lrn)
## the result "res" is a list, storing information on the actual "run", the
## corresponding benchmark result "bmr" and the applied "flow"
