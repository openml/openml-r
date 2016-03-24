---
title: "Example workflow using OpenML and mlr"
author: "The OpenML R Team"
date: "2016-03-18"
output: rmarkdown::html_vignette
bibliography: Bib.bib
vignette: >
  %\VignetteIndexEntry{OpenML}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


In this vignette we illustrate the advantages of OpenML by performing a small comparison study between a random forest and bagged trees.
We first create the respective binary classification learners using mlr, then query OpenML for suitable data sets, apply the learners on the data, and finally evaluate the results.

## Create Learners {#learners}

Because there is a variety of tree implementations in R, we select three implementations of different algorithms. These are **rpart** from the package *rpart* [@rpart] as an implementation of *CART*, **J48** from the package *RWeka* [@RWeka] as an implementation of *C4.5*, and **ctree** from the package *party* [@party], which is an implementation of the algorithm *Conditional inference trees*. As for the *Random forest*, we use the implementation **ranger** from the package *ranger* [@ranger].

While the random forest learner can be used as-is, the trees can conveniently be combined using mlr's bagging wrapper.
The number of trees is set to 100 for both the forest and all bagged tree learners so that this parameter does not influence the results.




```r
# create a random forest learner and three bagged tree learners
lrn1 = makeLearner("classif.ranger", num.trees = 100)
lrn2 = makeBaggingWrapper(makeLearner("classif.rpart"), bw.iters = 100)
lrn3 = makeBaggingWrapper(makeLearner("classif.J48"), bw.iters = 100)
lrn4 = makeBaggingWrapper(makeLearner("classif.ctree"), bw.iters = 100)
```

## Query OpenML

Now we search for appropriate tasks on OpenML by querying the server using `listOMLTasks()`, which returns a large data frame:

```r
all.tasks = listOMLTasks()
dim(all.tasks)
```

```
## [1] 8412   20
```

For this study the candidates are filtered to meet the following criteria:  
1. Binary classification problem  
2. 10-fold Cross-validation for validation  
3. No missing values -- Random Forest cannot handle them automatically  
4. Less than 1000 instances -- keep evaluation time low  
5. $n < p$  
6. Predictive accuracy as evaluation measure  

Although a data frame can be filtered with the `subset()` function (in R's base package), we strongly recommend the faster and more convenient alternatives provided by either **data.table** [@data.table] or **dplyr** [@dplyr].

```r
library(data.table)
tasks = as.data.table(all.tasks)
tasks = tasks[
  task.type == "Supervised Classification" &
  NumberOfClasses == 2 &
  estimation.procedure == "10-fold Crossvalidation" &
  NumberOfMissingValues == 0 &
  NumberOfInstances < 1000 &
  NumberOfNumericFeatures < NumberOfInstances &
  evaluation.measures == "predictive_accuracy", ]

nrow(tasks)
```

```
## [1] 257
```
We randomly pick 20 out of the 257 remaining tasks to keep the runtimes reasonable.
<!-- Furthermore, we left join the data frame returned by `listOMLDataSets()` to have a quick glance at the names of the selected data sets: #FIXME: Why not only call `tasks[, name]`?-->  
Furthermore, we have a quick glance at the names of the corresponding data sets.

```r
set.seed(34589)
tasks = tasks[sample(nrow(tasks), 20), ]
# tasks[listOMLDataSets(), name, on = "did", nomatch = 0]
tasks[, name]
```

```
##  [1] "SPECTF"                   "boston_corrected"        
##  [3] "analcatdata_vineyard"     "fri_c4_100_25"           
##  [5] "oil_spill"                "pwLinear"                
##  [7] "fri_c4_500_100"           "collins"                 
##  [9] "parkinsons"               "rabe_97"                 
## [11] "analcatdata_germangss"    "analcatdata_election2000"
## [13] "sleuth_ex2015"            "fri_c0_250_5"            
## [15] "stock"                    "rmftsa_ladata"           
## [17] "fri_c1_250_5"             "fri_c0_500_5"            
## [19] "analcatdata_apnea2"       "fri_c0_100_10"
```

## Evaluation  

The function `runTaskMlr()` applies an mlr learner on an OpenML data set and returns a benchmark result. Here, we write a short helper function that extracts the aggregated performance measure from this benchmark result for a given task ID and a learner ID determining which of the four learners is run. We then generate a grid of these IDs and map them to the helper function.

```r
runTask = function(task.id, learner.id) {
  res = runTaskMlr(getOMLTask(task.id), learners[[learner.id]])
  getBMRAggrPerformances(res$bmr)[[1]][[1]][1]
}
learners = list(lrn1, lrn2, lrn3, lrn4)
grid = expand.grid(task.id = tasks$task.id, learner.id = 1:4)
res = Map(runTask, task.id = grid$task.id, learner.id = grid$learner.id)
```


The next figure shows the differences in predictive accuracy per task between each combination of two learners.
Besides boxplots, there are also so-called violin plots depicted in the background, that represent the estimated density of the differences' distributions.
![plot of chunk figure](figure/figure-1.png)![plot of chunk figure](figure/figure-2.png)

## References  
