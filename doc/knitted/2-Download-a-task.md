Download a task
===============

Each OpenML task is a bundle of a data set, a target feature, an estimation procedure (e.g., 10-fold CV) and one or more evaluation measures. Every task has a type, e.g., Supervised Classification or Supervised Regression. A user can download such a task from the OpenML server, compute predictions with an algorithm (called "flow") and upload this algorithm as well as the predictions. The server will then calculate many different measures and add them to the data base.

### Download an OpenML task
To download a certain task from the OpenML server, you need to know the task's ID. You can look for appropriate task IDs [here](http://openml.org/search?q=&type=task). See [section 6](6-Browse-the-database.md) to learn how to search for tasks via R.

The following call returns an OpenML task object: 


```splus
task = downloadOpenMLTask(id = 4)
print(task)
```

Note: You do not have to be registered at http://openml.org/ to download a task. 

### Download an OpenML data set as an mlr task
OpenML tasks have predefined estimation procedures and measures. Sometimes you might want to deviate from these fixings. Of course, it is possible to define new tasks that match your desires, but this will not always be the means of choice -- e.g., when you want to run a few preliminary experiments. For this matter, you can use the function `downloadOpenMLDataAsMlrTask` if you are working with [mlr](https://github.com/berndbischl/mlr). By the following example call, the Iris data set is downloaded and automatically converted into an mlr (classification) task: 


```splus
mlr.task = downloadOpenMLDataAsMlrTask(name = "iris")
```

Now you can freely apply any resampling procedure. For further information on how this works, see [here](http://berndbischl.github.io/mlr/man/makeResampleDesc.html).

Sometimes data sets have more than one version. The default is to download the first version of a data set but a different version can be selected easily:


```splus
mlr.task = downloadOpenMLDataAsMlrTask(name = "iris", version = 2)
```

It is recommended to download the first version if you do not have a clue what is different (or better) in a later version of the data set.
----------------------------------------------------------------------------------------------------------------------
Jump to:    
[1 Introduction](1-Introduction.md)    
2 Download a task  
[3 Upload an implementation](3-Upload-an-implementation.md)  
[4 Upload predictions](4-Upload-predictions.md)  
[5 Download performance measures](5-Download-performance-measures.md)  
[6 Browse the database](6-Browse-the-database.md)
