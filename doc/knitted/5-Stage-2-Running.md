Stage 2 - Running 
=================

You can apply an implementation of an algorithm (which is also called flow) to a specific task. There are several possibilities to do this.

### Run a task using a downloaded OpenML flow

A downloaded flow contains an `.R` file that provides a `sourcedFlow` function, which can be applied to a specific task:


```r
source(flow$source.path)
run.flow = sourcedFlow(task.id = 1L)
run.flow
```

### Run a task with a specified mlr learner

If you are working with [mlr](https://github.com/berndbischl/mlr), you can specify a `RLearner` object and use the function `runTaskMlr` to create the desired `OMLMlrRun` object. 


```r
library(mlr)
lrn = makeLearner("classif.rpart")
run.mlr = runTaskMlr(task, lrn)
run.mlr
```

### Run a task without using mlr

If you are not using `mlr`, you will have to invest quite a bit more time to get things done. So -- unless you have good reasons to do otherwise -- we strongly encourage you to use `mlr`. 

The following example shows how to create an OpenML flow description object manually.

The first step is to create a list of `OMLFlowParameter`s, where each parameter of your implementation is stored. Let's assume we have written an algorithm that has two parameters called "a" (with default value: 500) and "b" (with default value: TRUE). 


```r
flow.par.a = makeOMLFlowParameter(
  name = "a", 
  data.type = "numeric", 
  default.value = "500",  # Yes, all defaults must be passed as strings.
  description = "An optional description of parameter a.")

flow.par.b = makeOMLFlowParameter(
  name = "b", 
  data.type = "logical", 
  default.value = "TRUE",  
  description = "An optional description of parameter b.")

flow.pars = list(flow.par.a, flow.par.b)
```

Now we can create the whole description object. Try to find a good name for your algorithm that gives other users an idea of what is happening.


```r
oml.flow = makeOMLFlow(
  name = "good_name",
  external.version = "1.0",
  description = "Please take some time and write a description of your algorithm/changes compared with the previous version/etc. here.",
  parameter = flow.pars)
```

Before you can apply the created flow to a task, you have to create a `OMLRun` object. If you want to change the parameter settings for the run, you can do this by a list that contains an `OMLRunParameter` objects for each parameter defined by the flow **whose setting varies from the default**. The class `OMLRunParameter` has the following members: 
* name
* value 
* component (optional and only needed if the parameter belongs to a (sub-)component of the
implementation. Then, the name of this component must be handed over here.)

Let's assume that we set the parameter "a" to a value of 300. Parameter "b" on the other hand remains in the
default setting, so that we do not need to define a `OMLRunParameter` for it:


```r
run.par.a = makeOMLRunParameter(name = "a", value = "300")  
run.pars = list(run.par.a)
```

Now you can create your `OMLRun` object using the `makeOMLRun` function. If you want to upload the run later, you will need to pass a `data.frame` for the `predictions` parameter, which have to be in a standardized form. The call `task$output$predictions` gives us the expected column names and their types. For supervised classification and regression tasks, these
are:
* repeat (integer)
* fold (integer)
* row_id (integer)   
* prediction (string)

and additionally, in case of a classification task:
* confidence.*classname_1* 
* confidence.*classname_2* 
* ... (one column for each level of the target variable).

**Note: The columns "repeat", "fold" and "row_id" have to be zero-based!** 

Example: An excerpt of predictions (Iris data set, 10-fold CV, 2 repeats).

        repeat fold row_id      prediction confidence.Iris-setosa confidence.Iris-versicolor confidence.Iris-virginica  
    1        0    0    140  Iris-virginica                      0                          0                         1  
    ...    ...  ...    ...             ...                    ...                        ...                       ...  
    51       0    3     37     Iris-setosa                      1                          0                         0  
    ...    ...  ...    ...             ...                    ...                        ...                       ...  
    150      0    9     76  Iris-virginica                      0                          0                         1  
    151      1    0    110  Iris-virginica                      0                          0                         1  
    ...    ...  ...    ...             ...                    ...                        ...                       ...  
    300      1    9     58 Iris-versicolor                      0                          1                         0  

When you have produced such a prediction and assigned it to a `preds` object, you can create the run by:


```r
run = makeOMLRun(task.id = 1L, parameter.setting = run.pars, predictions = preds)  
```

----------------------------------------------------------------------------------------------------
Jump to:   
[Introduction](1-Introduction.md)  
[Configuration](2-Configuration.md)  
[Stage 0 - Listing](3-Stage-0-Listing.md)  
[Stage 1 - Downloading](4-Stage-1-Downloading.md)  
Stage 2 - Running models on tasks  
[Stage 3 - Uploading](6-Stage-3-Uploading.md)  
[Example workflow with mlr](8-Example-workflow-with-mlr.md)
