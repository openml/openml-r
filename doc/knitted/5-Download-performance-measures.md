Download performance measures
=============================

The server computes several performance measures (metrics) for every run that is uploaded and stores these. This makes it possible to easily compare your results to the results of others who have worked
on the same task. 

### Download run results
To download the results of one of your own runs, you have to know the corresponding run ID, which is returned by `uploadOpenMLRun`. In [section 4](4-Upload-predictions.md), we uploaded a run and
retrieved a unique run id that we called "run.ul". In order to get all stored metrics for this
specific run, we can use the function `downloadOpenMLRunResults`:


```splus
run.results = downloadOpenMLRunResults(run.ul)
run.results
```

```
## 
## Run Results :: (Run ID = 35063, Task ID = 4)
## 	Flow ID:  653
## 	User ID:  212
```
You can print the metrics to the console to get a first look:

```splus
print(run.results, printMetrics = TRUE)
```
The so called "array.data" in which matrices and vectors are stored is not printed to keep
the output readable. The complete data.set containing all information is accessible as follows:

```splus
metrics = run.results$output.data$evaluation
```
### Download task results
It is possible to download all stored metrics of all runs of a certain task at the same time. This
might be very useful in order to compare the performance of many different implementations. To
download all the results of a task, you only have to know the task ID. 


```splus
task.results = downloadOpenMLTaskResults(id = 4)
task.results
```

```
## 
## Task Results :: (Task ID = 4, Data ID = 4)
## 	Task Type ID        :  1
## 	Estimation Procedure:  10-fold Crossvalidation
```
For most tasks, there are a lot of runs, so the metrics data.frame might be very large. Thus, it
rarely makes sense to print the evaluations to the console (which is possible nevertheless, just
like in the example above with `printMetrics = TRUE`). You can access the whole data.frame with:

```splus
task.metrics = task.results$metrics
```

----------------------------------------------------------------------------------------------------
Jump to:   
[1 Introduction](1-Introduction.md)  
[2 Download a task](2-Download-a-task.md)  
[3 Upload an implementation](3-Upload-an-implementation.md)  
[4 Upload predictions](4-Upload-predictions.md)  
5 Download performance measures  
[6 Browse the database](6-Browse-the-database.md)
