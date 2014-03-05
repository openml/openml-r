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

runExperiments <- function(reg, task.low, task.high, lrn.low, lrn.high, 
  res=list(walltime=3600, memory=4*1024), wait=function(retries) 100, max.retries=10) {
   
  # get the task IDs of all classification tasks
  classif.task.ids <- runSQLQuery("select task_id from task where ttid = 1")
  
  for(id in classif.task.ids[task.low:task.high]) {
    pars <- list(id = id)
    task.design <- makeDesign("task", exhaustive=pars)
    task <- downloadOpenMLTask(id)
    
    pars <- try(listLearnersForTask(toMLR(task)$mlr.task))
    
    if(is.error(pars) || length(pars) == 0 || length(pars) < lrn.low){
      next
    } else {
      if(missing(lrn.high))
        this.lrn.high <- length(pars)
      else
        this.lrn.high <- lrn.high
      
      if(length(pars) < this.lrn.high) 
        this.lrn.high <- length(pars)
    }
    pars <- list(lrn = pars[lrn.low:this.lrn.high])
    lrn.design <- makeDesign("lrn", exhaustive=pars)
    
    addExperiments(reg, prob.designs=task.design, algo.designs=lrn.design)
  }
  
  submitJobs(reg, resources=res, wait=wait, max.retries=max.retries)
}
