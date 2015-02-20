# research question: what is better, lda or tree?
#

library(mlr)
load_all()

authenticateUser()

tchars = listOMLTasks(type = 1L)
tchars2 = subset(tchars, NumberOfInstances <= 500L & NumberOfFeatures <= 10L &
  NumberOfInstancesWithMissingValues == 0L & NumberOfClasses <= 4L)

tchars3 = head(tchars2, 1L)

###################### cut1: we now have taks ids


flows = list(
  list(lrn = makeLearner("classif.lda"), flow.id = NA),
  list(lrn = makeLearner("classif.rpart"), flow.id = NA)
)

###################### cut2: we now have learners, this is indep oof server anyway

# # register our flows
# for (j in seq_along(flows)) {
#   id = uploadOMLFlow(flows[[j]]$lrn)
#   flows[[j]]$flow.id = id
# }

# myexps = data.frame()


# # FIXME: list my runs

# for (i in seq_row(tchars3)) {
#   tch = tchars3[i,]
#   otask = getOMLTask(tch$task.id)
#   for (j in seq_along(flows)) {
#     f = flows[[j]]
#     res = runTaskMlr(f$lrn, task = otask)
#     # we are done, upload pred now
#     rid = uploadOMLRun(res, implementation.id = f$flow.id)
#     # FIXME: the package should maybe support stuff like this
#     myexps = rbind(myexps, data.frame(lrn = f$lrn$id, flow.id = f$flow.id, run.id = rid))
#   }
# }


# FIXME: function to get results based on run id collection

runs = lapply(myexps$run.id, getOMLRun)
evals = lapply(runs, function(x) x$output.data$evaluations)

# FIXME: check measures values againts mlr




