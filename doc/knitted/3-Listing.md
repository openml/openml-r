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
##       name MajorityClassSize MaxNominalAttDistinctValues MinorityClassSize
## 1   anneal               684                          10                 0
## 2   anneal               684                           9                 0
## 3 kr-vs-kp              1669                           3              1527
```

```r
inactive.data = listOMLDataSets(status = "deactivated")  # returns deactivated data sets
inactive.data[1:3, 3:6]
```

```
##      name MajorityClassSize MaxNominalAttDistinctValues MinorityClassSize
## NA   <NA>                NA                          NA                NA
## NA.1 <NA>                NA                          NA                NA
## NA.2 <NA>                NA                          NA                NA
```

To find a specific data set, one can now query the resulting `datasets` object. Suppose we want
to find the `iris` data set.


```r
subset(datasets, name == "iris")[, 1:6]
```

```
##     did status name MajorityClassSize MaxNominalAttDistinctValues
## 55   61 active iris                50                          -1
## 821 969 active iris               100                          -1
##     MinorityClassSize
## 55                 50
## 821                50
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
head(tasks[, 1:5])
```

```
##   task.id                 task.type did status       name
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
head(subset(tasks, task.type == "Supervised Classification" & did == 61L)[, 1:5])
```

```
##      task.id                 task.type did status name
## 53        59 Supervised Classification  61 active iris
## 271      289 Supervised Classification  61 active iris
## 442     1823 Supervised Classification  61 active iris
## 551     1939 Supervised Classification  61 active iris
## 598     1992 Supervised Classification  61 active iris
## 4445    7306 Supervised Classification  61 active iris
```
### List flows

A flow is the definition and implementation of a specific algorithm workflow or script.
I.e., a flow is essentially the code that implements the algorithm.


```r
flows = listOMLFlows()
flows[56:63, 1:2]
```

```
##    flow.id             full.name
## 56      56         weka.ZeroR(1)
## 57      57          weka.OneR(1)
## 58      58    weka.NaiveBayes(1)
## 59      59          weka.JRip(1)
## 60      60           weka.J48(1)
## 61      61       weka.REPTree(1)
## 62      62 weka.DecisionStump(1)
## 63      63 weka.HoeffdingTree(1)
```

### List runs and run results

A run is a combination of a setup (**??? FIXME??? Setup is not explained (ans why should one use it if it cannot be queried?**) and a task. The results are stored as a run result.
Both, runs and run results can be listed. Here one has additional arguments to subset
the result. For example one can search for a specific `task.id`, `setup.id` or `implementation.id`.
To list all runs for [task 59](http://www.openml.org/t/59) one can use


```r
runs = listOMLRuns(task.id = 59L)  # must be restricted to a task, setup and/or implementation ID
head(runs)
```

```
##   run.id task.id setup.id flow.id uploader error.message
## 1 478830      59     2417    1817       64            NA
## 2 479006      59     2417    1817       64            NA
## 3 479036      59     2417    1817       64            NA
## 4 479464      59     2417    1817       64            NA
## 5 479642      59     2417    1817       64            NA
## 6 479643      59     2417    1817       64            NA
```

```r
runresults = listOMLRunEvaluations(task.id = 59L)  # a task ID must be supplied
colnames(runresults)
```

```
##  [1] "run.id"                        "task.id"                      
##  [3] "setup.id"                      "flow.id"                      
##  [5] "area.under.roc.curve"          "area.under.roc.curve.array"   
##  [7] "average.cost"                  "confusion.matrix"             
##  [9] "f.measure"                     "f.measure.array"              
## [11] "kappa"                         "kb.relative.information.score"
## [13] "mean.absolute.error"           "mean.prior.absolute.error"    
## [15] "number.of.instances"           "number.of.instances.array"    
## [17] "precision"                     "precision.array"              
## [19] "predictive.accuracy"           "prior.entropy"                
## [21] "recall"                        "recall.array"                 
## [23] "relative.absolute.error"       "root.mean.prior.squared.error"
## [25] "root.mean.squared.error"       "root.relative.squared.error"  
## [27] "total.cost"                    "os.information"               
## [29] "scimark.benchmark"             "scimark.benchmark.array"      
## [31] "usercpu.time.millis"           "usercpu.time.millis.testing"  
## [33] "usercpu.time.millis.training"
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
