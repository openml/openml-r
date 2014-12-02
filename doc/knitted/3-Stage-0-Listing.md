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
##                    name
## 1  area_under_roc_curve
## 2          average_cost
## 3        build_cpu_time
## 4          build_memory
## 5      class_complexity
## 6 class_complexity_gain
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
```

Other functions, however, list only those entities that match one or more criteria:


```splus
tasks1 = listOMLTasks(type = 1L)  # lists only tasks of a certain task type (here: "Supervised Classification")
head(tasks1)
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
head(runresults)
```

```
##   run.id setup.id implementation.id          implementation task.id
## 1     62       17                76 weka.Bagging_REPTree(1)       1
## 2    237        4                59            weka.JRip(1)       1
## 3    359       12                67     weka.BayesNet_K2(1)       1
## 4    413       10                65    weka.RandomForest(1)       1
## 5    500       15                74        weka.Logistic(1)       1
## 6    517       13                70  weka.SMO_PolyKernel(1)       1
##   task.type.id              estim.proc area.under.roc.curve
## 1            1 10-fold Crossvalidation               0.9950
## 2            1 10-fold Crossvalidation               0.9789
## 3            1 10-fold Crossvalidation               0.9921
## 4            1 10-fold Crossvalidation               0.9986
## 5            1 10-fold Crossvalidation               0.9968
## 6            1 10-fold Crossvalidation               0.9668
##                                                                              confusion.matrix
## 1  [[5,0,2,0,1,0],[0,98,1,0,0,0],[2,2,676,0,0,4],[0,0,0,0,0,0],[0,0,0,0,67,0],[0,0,2,0,0,38]]
## 2  [[3,1,4,0,0,0],[0,99,0,0,0,0],[0,5,677,0,0,2],[0,0,0,0,0,0],[0,0,0,0,67,0],[0,0,2,0,0,38]]
## 3 [[7,0,1,0,0,0],[1,95,3,0,0,0],[1,7,656,0,0,20],[0,0,0,0,0,0],[0,0,0,0,67,0],[0,0,2,0,0,38]]
## 4  [[7,0,1,0,0,0],[0,99,0,0,0,0],[0,2,680,0,0,2],[0,0,0,0,0,0],[0,0,0,0,67,0],[0,0,3,0,0,37]]
## 5  [[6,1,1,0,0,0],[0,99,0,0,0,0],[0,0,678,0,0,6],[0,0,0,0,0,0],[0,0,0,0,67,0],[0,0,1,0,1,38]]
## 6 [[7,0,1,0,0,0],[0,97,2,0,0,0],[1,7,674,0,0,2],[0,0,0,0,0,0],[0,0,0,0,67,0],[0,0,11,0,0,29]]
##   f.measure  kappa kb.relative.information.score mean.absolute.error
## 1    0.9843 0.9612                         830.7            0.009306
## 2    0.9832 0.9610                         822.7            0.007985
## 3    0.9632 0.9066                         787.8            0.016386
## 4    0.9910 0.9777                         830.3            0.011131
## 5    0.9889 0.9724                         852.9            0.003554
## 6    0.9725 0.9326                        -451.2            0.222841
##   mean.prior.absolute.error number.of.instances
## 1                    0.1343                 898
## 2                    0.1343                 898
## 3                    0.1343                 898
## 4                    0.1343                 898
## 5                    0.1343                 898
## 6                    0.1343                 898
##                                                        os.information
## 1 [ Oracle Corporation, 1.7.0_51, amd64, Linux, 3.7.10-1.28-desktop ]
## 2 [ Oracle Corporation, 1.7.0_51, amd64, Linux, 3.7.10-1.28-desktop ]
## 3 [ Oracle Corporation, 1.7.0_51, amd64, Linux, 3.7.10-1.28-desktop ]
## 4 [ Oracle Corporation, 1.7.0_51, amd64, Linux, 3.7.10-1.28-desktop ]
## 5 [ Oracle Corporation, 1.7.0_51, amd64, Linux, 3.7.10-1.28-desktop ]
## 6 [ Oracle Corporation, 1.7.0_51, amd64, Linux, 3.7.10-1.28-desktop ]
##   precision predictive.accuracy prior.entropy recall
## 1    0.9843              0.9844         1.215 0.9844
## 2    0.9848              0.9844         1.215 0.9844
## 3    0.9682              0.9610         1.215 0.9610
## 4    0.9911              0.9911         1.215 0.9911
## 5    0.9895              0.9889         1.215 0.9889
## 6    0.9731              0.9733         1.215 0.9733
##   relative.absolute.error root.mean.prior.squared.error
## 1                 0.06929                        0.2582
## 2                 0.05945                        0.2582
## 3                 0.12201                        0.2582
## 4                 0.08288                        0.2582
## 5                 0.02646                        0.2582
## 6                 1.65921                        0.2582
##   root.mean.squared.error root.relative.squared.error  scimark.benchmark
## 1                 0.06371                      0.2467 1990.0545716446845
## 2                 0.07048                      0.2730 2009.3945079911828
## 3                 0.09863                      0.3820 2002.6266133146732
## 4                 0.05893                      0.2282 2008.8406703096214
## 5                 0.05695                      0.2206  2004.272840984903
## 6                 0.31115                      1.2050 2009.1159463885701
##   build.cpu.time build.memory average.cost total.cost
## 1             NA         <NA>           NA         NA
## 2             NA         <NA>           NA         NA
## 3             NA         <NA>           NA         NA
## 4             NA         <NA>           NA         NA
## 5             NA         <NA>           NA         NA
## 6             NA         <NA>           NA         NA
##   usercpu.time.millis.testing usercpu.time.millis.training
## 1                          NA                           NA
## 2                          NA                           NA
## 3                          NA                           NA
## 4                          NA                           NA
## 5                          NA                           NA
## 6                          NA                           NA
```
