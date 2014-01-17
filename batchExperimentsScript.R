library(BatchExperiments)
library(mlr)
library(stringr)
devtools::load_all("openML")

reg <- makeExperimentRegistry(id="openML_with_mlr", packages=c("mlr", "RCurl", "XML", "stringr"))

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
    return(NA)
  } else {
    return(res$mlr.resample.results$aggr)
  }
}

addAlgorithm(reg, id="lrn", fun=resample.lrn)

# get the task IDs of all classification tasks
classif.task.ids <- openMLSQLQuery("select task_id from task where ttid = 1")
classif.task.ids <- as.numeric(as.character(classif.task.ids$task_id))

# get the names of all classification learners
all.lrners <- listLearners(type = "classif")

pars <- list(id = classif.task.ids)
task.design <- makeDesign("task", exhaustive=pars)
pars <- list(lrn = all.lrners)
lrn.design <- makeDesign("lrn", exhaustive=pars)

addExperiments(reg, prob.designs=task.design, algo.designs=lrn.design)

submitJobs(reg, resources=list(walltime=3600, memory=4*1024),
  wait=function(retries) 100, max.retries=10)

# get information about the expired experiments
#exp.ids <- findExpired(reg)
#exp.jobs <- getJobs(reg, exp.ids)
#exp.data.set.ids <- unlist(lapply(exp.jobs, function(x) x$prob.pars$id))
#exp.lrners <- unlist(lapply(exp.jobs, function(x) x$algo.pars$lrn))
#exp.info <- data.frame(exp.data.set.ids, exp.lrners)

#submitJobs(reg, resources=list(walltime=7200, memory=8*1024),
#  wait=function(retries) 100, max.retries=10, ids=exp.ids)

# get the results
# all.res <- reduceResults(reg, 
#   fun = function(aggr, job, res) rbind(aggr, c(job$algo.pars, job$prob.pars, res)), init = c())

# sep.res <- list()
# for(i in seq_along(all.lrners)) {
#   inds <- which(unlist(all.res[, 1]) == all.lrners[i])
#   sep.res[[i]] <- unlist(all.res[inds, 3])
# }

# summaries for all learners
# sum.res <- lapply(sep.res, summary)
# names(sum.res) <- all.lrners

# fractions of NAs for each learner
# frac.NA <- data.frame(lrn = all.lrners, frac_NA = unlist(lapply(sep.res, function(x) sum(is.na(x))/length(x))))
# frac.NA[order(frac.NA[, 2]), ]