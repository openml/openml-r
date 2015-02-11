Example workflow with mlr
=========================

Here we will show you how a standard workflow could look like. This just a small example, to get
further information about the package and its functions, please have a look at the other sections
of this tutorial or the [detailed documentation](http://www.rdocumentation.org/packages/OpenML).

For this example, let's assume we wanted to test how good the R implementation of lda (package 
`MASS`) is, in comparison with all uploaded runs on 20 tasks. We only want to consider tasks
that have less than 10 features, not more than 100 instances, exactly 2 classes in the target
feature and not a single missing value. Then, we have to upload the learner, compute predictions and
upload these.


```r
library(mlr)

tl = listOMLTasks()

# filter data sets and get appropriate data set IDs:

# find tasks that match our desires and randomly select 20 of them
task.ids = = subset(tl, NumberOfFeatures < 10 & NumberOfInstances < 100 & NumberOfClasses == 2 & 
  NumberOfMissingValues == 0, select = task.id, drop = TRUE)
task.ids = sample(task.ids, 20)

# get a valid session hash:
hash = authenticateUser("openml.rteam@gmail.com", "testpassword")

# create the mlr learner for lda:
lrn = makeLearner("classif.lda")

# upload the learner and retrieve its flow ID:
flow.id = uploadMlrLearner(lrn, hash) 

run.ids = c()
for (id in task.ids) {
  task = getOMLTask(id)
  res = try(runTask(task, lrn))  # try to compute predictions with our learner
  if (is.error(res)) {
    # if an error occured, upload the error message:
    run.id = uploadOMLRun(task, lrn, flow.id, error.msg = res[1], session.hash = hash)
  } else {
    # else, upload the predictions:
    run.id = uploadOMLRun(task, lrn, flow.id, predictions = res, session.hash = hash)
  } 
  run.ids = c(run.ids, run.id)
}

## Compute the quantiles of the measure "predictive_accuracy" of all runs using our lda  
## implementation in comparison to the measures of different implementations. Quantiles next to 1  
## correspond to "lda has achieved (one of) the best results", quantiles next to 0 correspond to  
## "the other flows were better".
qs = c()
for (id in task.ids) {
  metrics = listOMLRunResults(id)$metrics
  if (is.null(metrics$predictive_accuracy))
    next
  f = ecdf(metrics$predictive_accuracy)
  q = f(metrics[metrics$impl.id == flow.id, "predictive_accuracy"])
  qs = c(qs, q)
}

boxplot(qs, ylim = c(0, 1), main = "Quantiles of lda measures")
```
![Boxplot of the quantiles](https://raw.githubusercontent.com/openml/r/master/doc/figures/boxplot_example.png)  
As we can see in the boxplot, the performance of lda varies quite strongly. Mostly, though, the lda
measures were better than the average of all run results on the considered tasks.

----------------------------------------------------------------------------------------------------
Jump to:   
[Introduction](1-Introduction.md)  
[Configuration](2-Configuration.md)  
[Stage 0 - Listing](3-Stage-0-Listing.md)  
[Stage 1 - Downloading](4-Stage-1-Downloading.md)  
[Stage 2 - Running models on tasks](5-Stage-2-Running.md)  
[Stage 3 - Uploading](6-Stage-3-Uploading.md)  
Example workflow with mlr
