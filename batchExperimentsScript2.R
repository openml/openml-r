# Get results on all classification tasks for all adequate (!) mlr learners
library(BatchExperiments)
library(mlr)
library(stringr)
devtools::load_all("openML")

reg <- makeExperimentRegistry(id="openML_with_mlr_2", packages=c("mlr", "RCurl", "XML", "stringr"))

getTask <- function(id) {
  devtools::load_all("openML")
  task <- downloadOpenMLTask(id)
  return(task)
}

addProblem(reg, id="task", dynamic=getTask)

resample.lrn <- function(static, dynamic, lrn) {
  devtools::load_all("openML")
  res <- try(runTask(dynamic, makeLearner(lrn), return.mlr.results = TRUE), silent=TRUE)
  if(is.error(res)) {
    message(res)
    return(res[1])
  } else {
    return(res$mlr.resample.results$aggr)
  }
}

addAlgorithm(reg, id="lrn", fun=resample.lrn)

# get the task IDs of all classification tasks
classif.task.ids <- runSQLQuery("select task_id from task where ttid = 1")
classif.task.ids <- as.numeric(as.character(classif.task.ids$task_id))

for(id in classif.task.ids[1:155]) {
  pars <- list(id = id)
  task.design <- makeDesign("task", exhaustive=pars)
  task <- downloadOpenMLTask(id)
  pars <- try(list(lrn = listLearnersForTask(toMLR(task)$mlr.task)))
  if(is.error(pars) || length(pars$lrn) == 0) 
    next
  lrn.design <- makeDesign("lrn", exhaustive=pars)
  
  addExperiments(reg, prob.designs=task.design, algo.designs=lrn.design)
}

submitJobs(reg, resources=list(walltime=3600, memory=4*1024),
  wait=function(retries) 100, max.retries=10)
