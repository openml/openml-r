Download a task
===============

Each OpenML task is a bundle of a data set, a target feature, an estimation procedure (e.g.,
10-fold CV) and one or more evaluation measures. Every task has a type, e.g., Supervised
Classification or Supervised Regression. A user can download such a task from the OpenML server,
compute predictions with an algorithm (called "flow") and upload this algorithm as well as the
predictions. The server will then calculate many different measures and add them to the data base.

### Get a valid session hash
To download a task and for most other functions, you will need a so-called session hash. If you created a configuration file containing username and password of your OpenML account, this is very simple:

```r
session.hash = authenticateUser()
```
Else, you have to pass your username und password here. **Your password will appear in plain text in your script/console!**

```r
session.hash = authenticateUser(email = "openml.rteam@gmail.com", password = "testpassword")
```

### Download an OpenML task
To download a certain task from the OpenML server, you need to know the task's ID. You can look for appropriate task IDs [here](http://openml.org/search?q=&type=task). See
[section 6](6-Browse-the-database.md) to learn how to search for tasks via R.

The following call returns an OpenML task object: 


```r
task = downloadOMLTask(id = 4, session.hash)
task
```

```
## 
## 	Task Type            : Supervised Classification
## 	Data Set             : labor :: (Version = 1, OpenML ID = 4)
## 	Target Feature(s)    : class
## 	Estimation Procedure : Stratified crossvalidation (1 x 10 folds)
## 	Evaluation Measure(s): predictive accuracy
```

### Download an OpenML data set only
OpenML tasks have predefined estimation procedures and measures. Sometimes you might want to deviate
from these fixings. Of course, it is possible to define new tasks that match your desires, but this
will not always be the means of choice -- e.g., when you want to run a few preliminary experiments.
For this matter, you can use the function `downloadOMLDataSet`, which downloads an
`OMLDataSet` including the corresponding data set description.


```r
oml.data = downloadOMLDataSet(1, session.hash)
oml.data
```

```
## 
## Data Set "anneal" :: (Version = 2, OpenML ID = 1)
## 	Default Target Attribute: class
```

If you are working with [mlr](https://github.com/berndbischl/mlr) (Machine Learning in R), you can
now go ahead and convert `oml.data` into an mlr (classification) task. Most data sets have a default target attribute. If this is not available or you want to select a different target feature, use the parameter `target`:


```r
mlr.task = toMlr(oml.data, target = "class")
mlr.task
```

```
## Supervised task: data
## Type: classif
## Target: class
## Observations: 898
## Features:
## numerics  factors  ordered 
##        6       32        0 
## Missings: FALSE
## Has weights: FALSE
## Has blocking: FALSE
## Classes: 5
##   1   2   3   5   U 
##   8  99 684  67  40 
## Positive class: NA
```

Now you can freely apply any resampling procedure. For further information on how this works, see
[here](http://berndbischl.github.io/mlr/man/makeResampleDesc.html).

----------------------------------------------------------------------------------------------------
Jump to:    
[1 Introduction](1-Introduction.md)   
[2 Configuration](2-Configuration.md)  
3 Download a task  
[4 Upload an implementation](4-Upload-an-implementation.md)  
[5 Upload predictions](5-Upload-predictions.md)  
[6 Download performance measures](6-Download-performance-measures.md)  
[7 Browse the database](7-Browse-the-database.md)  
[8 Example workflow with mlr](8-Example-workflow-with-mlr.md)
