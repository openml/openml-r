Stage 2 - Running Models on Tasks
=================================

### Run a task using a downloaded OpenML flow

A downloaded flow can be applied to a specific task by sourcing the `.R` file that provides a `sourcedFlow` function:


```r
source(flow$source.path)
run.flow = sourcedFlow(task.id = 1L)
run.flow
```

### Run a task with a specified mlr learner

If you are working with [mlr](https://github.com/berndbischl/mlr), you can specify a `RLearner` object using the `mlr` package and use the function `runTaskMlr` to store the desired `OMLMlrRun` object.


```r
library(mlr)
lrn = makeLearner("classif.rpart")
run.mlr = runTaskMlr(task, lrn)
run.mlr
```

<!-- ### Run a task without using mlr

If you are not using mlr, you will have to invest quite a bit more time to get things done. So -- unless you have good reasons to do otherwise -- we strongly encourage you to use mlr. 

The following example shows how to create an OpenML implementation description object manually.

The first step is to create a list of `OMLRunParameter`s, where each parameter of your implementation is stored. Let's assume we have written an algorithm that has two parameters called "a" (with default value: 500) and "b" (with default value: TRUE). 


```r
run.par = list(makeOMLRunParameter(name = "a", value = "500"),
  makeOMLRunParameter(name = "b", value = "TRUE"))
```


```r
run = makeOMLRun(task.id = 1L, parameter.setting = run.par)
```

Now we finally have the implementation description object. The last step is to write an R-script with your flow. Let's assume you have done this and have a string `sourcefile` containing the path to your script. Your flow can now be uploaded as follows: --> 

----------------------------------------------------------------------------------------------------
Jump to:   
[Introduction](1-Introduction.md)  
[Configuration](2-Configuration.md)  
[Stage 0 - Listing](3-Stage-0-Listing.md)  
[Stage 1 - Downloading](4-Stage-1-Downloading.md)  
Stage 2 - Running models on tasks  
[Stage 3 - Uploading](6-Stage-3-Uploading.md)  
[Example workflow with mlr](8-Example-workflow-with-mlr.md)
