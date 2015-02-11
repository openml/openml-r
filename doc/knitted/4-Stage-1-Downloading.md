Stage 1 - Downloading
=====================

### Download an OpenML task
Each OpenML task is a bundle of a data set, a target feature, an estimation procedure (e.g.,
10-fold CV), data splits fitting this estimation procedure and, finally, one or more evaluation measures. Every task has a type, e.g., Supervised Classification or Supervised Regression. A user can download such a task from the OpenML server, compute predictions with an algorithm (called "flow" or "implementation") and upload this algorithm as well as the predictions. The server will then calculate many different measures and add them to the data base.

To download a certain task from the OpenML server, you need to know the task's ID. See
[section 3](3-Stage-0-Listing.md) to learn how to retrieve a list of all available tasks in R.

The following call returns an OpenML task object: 


```r
task = getOMLTask(task.id = 1L)
```

```
## Error in getSessionHash(): Please authenticate first.
```

```r
task
```

```
## Error in eval(expr, envir, enclos): Objekt 'task' nicht gefunden
```
The corresponding data set can be accessed by

```r
str(task$data.set$data)
```

```
## Error in str(task$data.set$data): Objekt 'task' nicht gefunden
```

### Download an OpenML data set only
OpenML tasks have predefined estimation procedures and measures. Sometimes you might want to deviate
from these fixings. Of course, it is possible to define new tasks that match your desires, but this
will not always be the means of choice -- e.g., when you want to run a few preliminary experiments.
For this matter, you can use the function `getOMLDataSet`, which accepts not only tasks (as seen in the section above) but also a data set ID as input:


```r
anneal.data = getOMLDataSet(x = 1L)  # the anneal data set has the data set ID (did = 1, see also previous section)
```

```
## Error in getSessionHash(): Please authenticate first.
```

```r
anneal.data
```

```
## Error in eval(expr, envir, enclos): Objekt 'anneal.data' nicht gefunden
```

```r
anneal.data = getOMLDataSet(x = task)  # the task defined above (with task.id = 1) uses the anneal data
```

```
## Error in getOMLDataSet(x = task): Objekt 'task' nicht gefunden
```

```r
anneal.data
```

```
## Error in eval(expr, envir, enclos): Objekt 'anneal.data' nicht gefunden
```

### Download an OpenML run
To download the results of one run including all server and user computed metrics, you have to know the corresponding run ID. You can download a single OpenML run with the `getOMLRun` function:


```r
run = getOMLRun(run.id = 1L)  # see ?OMLRun for each slot of the OMLRun object
```

```
## Error in getSessionHash(): Please authenticate first.
```

Some important slots for the `OMLRun` object are:


```r
run$parameter.setting  # A list containing information on the parameter settings.
```

```
## Error in eval(expr, envir, enclos): Objekt 'run' nicht gefunden
```

```r
run$input.data  # All data that served as input for the run, including the URL to the data.
```

```
## Error in eval(expr, envir, enclos): Objekt 'run' nicht gefunden
```

```r
run$output.data$evaluations
```

```
## Error in eval(expr, envir, enclos): Objekt 'run' nicht gefunden
```

To retrieve predictions of an uploaded run, you can set the parameter `get.predictions = TRUE` to store the
predictions in the `$predictions` slot or use the function `getOMLPredictions(run)`:


```r
run.pred = getOMLRun(run.id = 1L, get.predictions = TRUE)
```

```
## Error in getSessionHash(): Please authenticate first.
```

```r
all.equal(run.pred$predictions, getOMLPredictions(run))
```

```
## Error in all.equal(run.pred$predictions, getOMLPredictions(run)): Objekt 'run.pred' nicht gefunden
```

### Download run results
UNDER CONSTRUCTION


----------------------------------------------------------------------------------------------------
Jump to:   
[Introduction](1-Introduction.md)  
[Configuration](2-Configuration.md)  
[Stage 0 - Listing](3-Stage-0-Listing.md)  
Stage 1 - Downloading  
[Stage 2 - Running models on tasks](5-Stage-2-Running.md)  
[Stage 3 - Uploading](6-Stage-3-Uploading.md)  
[8 Example workflow with mlr](8-Example-workflow-with-mlr.md)
