Listing
=======

In this stage, we want to list basic information about the various OpenML objects such as data sets, tasks, flows, runs, run results, evaluation measures or task types. See the [OpenML introduction](http://openml.org/guide) for an overview on and explanations of the different objects.

For each of these objects we have a function to query the information beginning with `listOML`. All of these functions return a `data.frame`, even when the result has only one column.

First, load the package:

```r
library("OpenML")
setOMLConfig(verbosity = 0) # switch off status output
```

### List data sets
To browse the OpenML data base for appropriate data sets, you can use `listOMLDataSets()`
in order to get basic data characteristics (number of features/instances/classes/missing values etc.)
for each data set. By default, `listOMLDataSets()` returns only data sets that have an active
status on OpenML. If you need data sets that are either `"in_preparation"` or `"deactivated"`,
you can change the `status` parameter:


```r
datasets = listOMLDataSets()  # returns active data sets
datasets[1:3, 3:6]
```

```
##       name NumberOfClasses NumberOfFeatures NumberOfInstances
## 1   anneal               6               39               898
## 2   anneal               6               39               898
## 3 kr-vs-kp               2               37              3196
```

```r
inactive.data = listOMLDataSets(status = "deactivated")  # returns deactivated data sets
inactive.data[1:3, 3:6]
```

```
##             name NumberOfClasses NumberOfFeatures NumberOfInstances
## 1        bridges               6               13               107
## 2        bridges               6               13               107
## 3 cylinder-bands               2               40               540
```

To find a specific data set, one can now query the resulting `datasets` object. Suppose we want
to find the `iris` data set.


```r
subset(datasets, name == "iris")[, 1:6]
```

```
##     did status name NumberOfClasses NumberOfFeatures NumberOfInstances
## 55   61 active iris               3                5               150
## 822 969 active iris               2                5               150
```

As one can see there are two data sets called `iris`. We want to use the original data set
with three classes. It has the data set ID `did` = 61.
One can also look at the data set on the OpenML web page
http://openml.org/d/61.

### List tasks
Each OpenML task is a bundle of a data set, a target feature, a (performance) estimation procedure (e.g.,
10-fold CV), data splits for this estimation procedure and, finally, one or more (performance) evaluation measures.
Every task has a type, e.g., `"Supervised Classification"` or `"Supervised Regression"`. To list tasks one can use

```r
tasks = listOMLTasks()
tasks[1:6, 1:5]
```

```
##   task_id                 task_type did status       name
## 1       1 Supervised Classification   1 active     anneal
## 2       2 Supervised Classification   2 active     anneal
## 3       3 Supervised Classification   3 active   kr-vs-kp
## 4       4 Supervised Classification   4 active      labor
## 5       5 Supervised Classification   5 active arrhythmia
## 6       6 Supervised Classification   6 active     letter
```

For some data sets, there may be more than one task available at the OpenML server.
For example, you can look for `"Supervised Classification"` tasks that are available
for a specific data set as follows:


```r
tasks = listOMLTasks()
# subset tasks to "Supervised Classification" for the iris data (did == 61)
subset(tasks, task_type == "Supervised Classification" & did == 61L)[, c(1, 6)]
```

```
##      task_id             estimation_procedure
## 53        59          10-fold Crossvalidation
## 271      289                  33% Holdout set
## 442     1823   5 times 2-fold Crossvalidation
## 551     1939 10 times 10-fold Crossvalidation
## 598     1992                    Leave one out
## 3892    6564          10-fold Crossvalidation
## 4436    7306          10-fold Crossvalidation
## 4451    7531          10-fold Crossvalidation
## 4459    7555          10-fold Crossvalidation
```
### List flows

A flow is the definition and implementation of a specific algorithm workflow or script.
I.e., a flow is essentially the code that implements the algorithm.


```r
flows = listOMLFlows()
flows[1:7, 1:2]
```

```
##   implementation.id                                   full.name
## 1                 1    openml.evaluation.EuclideanDistance(1.0)
## 2                 2     openml.evaluation.PolynomialKernel(1.0)
## 3                 3            openml.evaluation.RBFKernel(1.0)
## 4                 4 openml.evaluation.area_under_roc_curve(1.0)
## 5                 5         openml.evaluation.average_cost(1.0)
## 6                 6       openml.evaluation.build_cpu_time(1.0)
## 7                 7         openml.evaluation.build_memory(1.0)
```

### List setups

A setup is the combination of a flow with specific parameter settings.

**FIXME: Add this once it is up and running**

### List runs and run results

A run is a combination of a setup and a task. The results are stored as a run result.
Both, runs and run results can be listed. Here one has additional arguments to subset
the result. For example one can search for a specific `task.id`, `setup.id` or `implementation.id`.
To list all runs for [task 59](http://www.openml.org/t/59).


```r
runs = listOMLRuns(task.id = 59L)  # must be restricted to a task, setup and/or implementation ID
head(runs)

runresults = listOMLRunResults(task.id = 59L)  # a task ID must be supplied
colnames(runresults)
```

### List evaluation measures and task types
To list further objects such as evaluation measures and task types one can simply use
the respective functions.


```r
measures = listOMLEvaluationMeasures()
measures[1:7, , drop = FALSE]
```

```
##                    name
## 1  area_under_roc_curve
## 2          average_cost
## 3        build_cpu_time
## 4          build_memory
## 5               c_index
## 6      class_complexity
## 7 class_complexity_gain
```

```r
tasktypes = listOMLTaskTypes()
tasktypes
```

```
##   id                                  name
## 1  1             Supervised Classification
## 2  2                 Supervised Regression
## 3  3                        Learning Curve
## 4  4 Supervised Data Stream Classification
## 5  5                            Clustering
## 6  6            Machine Learning Challenge
## 7  7                     Survival Analysis
```


----------------------------------------------------------------------------------------------------
Jump to:
- [Introduction](1-Introduction.md)
- [Configuration](2-Configuration.md)
- Listing
- [Downloading](4-Downloading.md)
- [Running models on tasks](5-Running.md)
- [Uploading](6-Uploading.md)
- [Example workflow with mlr](7-Example-workflow-with-mlr.md)
