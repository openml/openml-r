Download a task
===============

Each OpenML task is a bundle of a data set, a target feature, an estimation procedure (e.g., 10-fold CV) and one or more evaluation measures. Every task has a type, e.g., Supervised Classification or Supervised Regression. A user can download such a task from the OpenML server, compute predictions with an algorithm (called "flow") and upload this algorithm as well as the predictions. The server will then calculate many different measures and add them to the data base.

### Download an OpenML task
To download a certain task from the OpenML server, you need to know the task's ID. You can look for appropriate task IDs [here](http://openml.org/search?q=&type=task). See [section 6](6-Browse-the-database.md) to learn how to search for tasks via R.

The following call returns an OpenML task object: 


```splus
task = downloadOpenMLTask(id = 4)
task
```

```
## 
## OpenML Task 4 :: (Data ID = 4)
## 	Task Type            : Supervised Classification
## 	Data Set             : labor :: (Version = 1, OpenML ID = 4)
## 	Target Feature(s)    : class
## 	Estimation Procedure : Stratified crossvalidation (1 x 10 folds)
## 	Evaluation Measure(s): predictive accuracy
```

Note: You do not have to be registered at http://openml.org/ to download a task. 

### Download an OpenML data set only
OpenML tasks have predefined estimation procedures and measures. Sometimes you might want to deviate from these fixings. Of course, it is possible to define new tasks that match your desires, but this will not always be the means of choice -- e.g., when you want to run a few preliminary experiments. For this matter, you can use the function `downloadOpenMLData`, which downloads an OpenMLDataSetDescription including the corresponding data set. Sometimes data sets have more than one version. The default is to download the first version of a data set but a different version can be selected easily with the argument `version`. It is recommended to download the first version if you do not know about what is different in a later version. 


```splus
oml.data = downloadOpenMLData(name = "iris")
oml.data
```

```
## 
## Data Set "iris" :: (Version = 1, OpenML ID = 61)
## 	Collection Date         : 1936
## 	Creator(s)              : R.A. Fisher
## 	Default Target Attribute: class
```

If you are working with [mlr](https://github.com/berndbischl/mlr), you can now go ahead and convert `oml.data` into an mlr (classification) task. Most data sets have a default target attribute. If this is not available or you want to select a different target feature, use the parameter `target`:


```splus
mlr.task = toMlr(oml.data, target = "class")
mlr.task
```

```
## Supervised task: data
## Type: classif
## Target: class
## Observations: 150
## Features:
## numerics  factors 
##        4        0 
## Missings: FALSE
## Has weights: FALSE
## Has blocking: FALSE
## Classes: 3
##     Iris-setosa Iris-versicolor  Iris-virginica 
##              50              50              50 
## Positive class: NA
```

Now you can freely apply any resampling procedure. For further information on how this works, see [here](http://berndbischl.github.io/mlr/man/makeResampleDesc.html).

----------------------------------------------------------------------------------------------------------------------
Jump to:    
[1 Introduction](1-Introduction.md)    
2 Download a task  
[3 Upload an implementation](3-Upload-an-implementation.md)  
[4 Upload predictions](4-Upload-predictions.md)  
[5 Download performance measures](5-Download-performance-measures.md)  
[6 Browse the database](6-Browse-the-database.md)
