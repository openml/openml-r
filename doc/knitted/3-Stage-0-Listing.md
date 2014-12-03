Stage 0 - Listing
=================

In this stage, we want to list basic information, e.g., about all data sets, tasks, flows, runs, run results, available evaluation measures or task types. For each of these purposes we have a function beginning with "listOML". All of them return a data.frame, even when the result has only one column.

### Get a valid session hash
To download a task and for most other functions, you will need a so-called session hash. If you created a configuration file containing username and password of your OpenML account, this is very simple:

```splus
session.hash = authenticateUser()
```
Else, you have to pass your username und password here. **Your password will appear in plain text in your script/console!**

```splus
session.hash = authenticateUser(email = "openml.rteam@gmail.com", password = "testpassword")
```

### Listing
Let's have a look at some examples. Most of the listing-functions do not have any parameters:


```splus
datasets = listOMLDataSets()
head(datasets)
```

```
##   did status NumberOfClasses NumberOfFeatures NumberOfInstances
## 1   1 active               6               39               898
## 2   2 active               6               39               898
## 3   3 active               2               37              3196
## 4   4 active               2               17                57
## 5   5 active              16              280               452
## 6   6 active              26               17             20000
##   NumberOfInstancesWithMissingValues NumberOfMissingValues
## 1                                  0                     0
## 2                                898                 22175
## 3                                  0                     0
## 4                                 56                   326
## 5                                384                   408
## 6                                  0                     0
##   NumberOfNumericFeatures
## 1                       6
## 2                       6
## 3                       0
## 4                       8
## 5                     206
## 6                      16
```

```splus
flows = listOMLFlows()
head(flows)
```

```
##   id                                   full.name
## 1  1    openml.evaluation.EuclideanDistance(1.0)
## 2  2     openml.evaluation.PolynomialKernel(1.0)
## 3  3            openml.evaluation.RBFKernel(1.0)
## 4  4 openml.evaluation.area_under_roc_curve(1.0)
## 5  5         openml.evaluation.average_cost(1.0)
## 6  6       openml.evaluation.build_cpu_time(1.0)
##                                     name version external.version uploader
## 1    openml.evaluation.EuclideanDistance       1                        NA
## 2     openml.evaluation.PolynomialKernel       1                        NA
## 3            openml.evaluation.RBFKernel       1                        NA
## 4 openml.evaluation.area_under_roc_curve       1                        NA
## 5         openml.evaluation.average_cost       1                        NA
## 6       openml.evaluation.build_cpu_time       1                        NA
```

```splus
measures = listOMLEvaluationMeasures()
head(measures)
```

```
##                   name
## 1 area_under_roc_curve
## 2         average_cost
## 3       build_cpu_time
## 4         build_memory
## 5              c_index
## 6     class_complexity
```

```splus
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

Other functions, however, list only those entities that match one or more criteria:


```splus
tasks = listOMLTasks(type = 1L)  # lists only tasks of a certain task type (here: "Supervised Classification")
head(tasks)
```

```
##   task.id                 task.type did status NumberOfClasses
## 1       1 Supervised Classification   1 active               6
## 2       2 Supervised Classification   2 active               6
## 3       3 Supervised Classification   3 active               2
## 4       4 Supervised Classification   4 active               2
## 5       5 Supervised Classification   5 active              16
## 6       6 Supervised Classification   6 active              26
##   NumberOfFeatures NumberOfInstances NumberOfInstancesWithMissingValues
## 1               39               898                                  0
## 2               39               898                                898
## 3               37              3196                                  0
## 4               17                57                                 56
## 5              280               452                                384
## 6               17             20000                                  0
##   NumberOfMissingValues NumberOfNumericFeatures
## 1                     0                       6
## 2                 22175                       6
## 3                     0                       0
## 4                   326                       8
## 5                   408                     206
## 6                     0                      16
```

```splus
runs = listOMLRuns(task.id = 1L)  # must be restricted to a task, setup and/or implementation ID
head(runs)
```

```
##   run.id task.id setup.id implementation.id uploader
## 1    614       1        1                56       16
## 2   6248       1        1                56       17
## 3  24094       1        1                56       16
## 4  24095       1        1                56       16
## 5  24688       1        1                56        1
## 6   6246       1        2                57       17
```

```splus
runresults = listOMLRunResults(task.id = 1L)  # a task ID must be supplied
colnames(runresults)
```

```
##  [1] "run.id"                        "setup.id"                     
##  [3] "implementation.id"             "implementation"               
##  [5] "task.id"                       "task.type.id"                 
##  [7] "estim.proc"                    "area.under.roc.curve"         
##  [9] "confusion.matrix"              "f.measure"                    
## [11] "kappa"                         "kb.relative.information.score"
## [13] "mean.absolute.error"           "mean.prior.absolute.error"    
## [15] "number.of.instances"           "os.information"               
## [17] "precision"                     "predictive.accuracy"          
## [19] "prior.entropy"                 "recall"                       
## [21] "relative.absolute.error"       "root.mean.prior.squared.error"
## [23] "root.mean.squared.error"       "root.relative.squared.error"  
## [25] "scimark.benchmark"             "build.cpu.time"               
## [27] "build.memory"                  "average.cost"                 
## [29] "total.cost"                    "usercpu.time.millis.testing"  
## [31] "usercpu.time.millis.training"
```

----------------------------------------------------------------------------------------------------
Jump to:   
[Introduction](1-Introduction.md)  
[Configuration](2-Configuration.md)  
Stage 0 - Listing  
[Stage 1 - Downloading](4-Stage-1-Downloading.md)  
[Stage 2 - Running models on tasks](5-Stage-2-Running.md)  
[Stage 3 - Uploading](6-Stage-3-Uploading.md)  
[8 Example workflow with mlr](8-Example-workflow-with-mlr.md)
