Stage 1 - Downloading
=====================

### Download an OpenML task
A user can download a task from the OpenML server, compute predictions with an algorithm (i.e., with a specific setup)
and upload this algorithm as well as the predictions. The server will then calculate many different performance measures 
and add them to the data base.

The following call returns an OpenML task object:


```r
task = getOMLTask(task.id = 59L)
task
```

```
## 
## OpenML Task 59 :: (Data ID = 61)
##   Task Type            : Supervised Classification
##   Data Set             : iris :: (Version = 1, OpenML ID = 61)
##   Target Feature(s)    : class
##   Estimation Procedure : Stratified crossvalidation (1 x 10 folds)
```

The corresponding `"OMLDataSet"` object can be accessed by


```r
task$input$data.set
```

```
## 
## Data Set "iris" :: (Version = 1, OpenML ID = 61)
##   Collection Date         : 1936
##   Creator(s)              : R.A. Fisher
##   Default Target Attribute: class
```

A special print function gives the basic information on the data set. To extract the data itself 
one can use


```r
iris.data = task$input$data.set$data
head(iris.data)
```

```
##   sepallength sepalwidth petallength petalwidth       class
## 0         5.1        3.5         1.4        0.2 Iris-setosa
## 1         4.9        3.0         1.4        0.2 Iris-setosa
## 2         4.7        3.2         1.3        0.2 Iris-setosa
## 3         4.6        3.1         1.5        0.2 Iris-setosa
## 4         5.0        3.6         1.4        0.2 Iris-setosa
## 5         5.4        3.9         1.7        0.4 Iris-setosa
```

### Download an OpenML data set only
To directly download a data set, e.g., when you want to run a few preliminary experiments, one can
use the function `getOMLDataSet`. The function accepts a data set ID as input and returns the corresponding `OMLDataSet`:


```r
iris.data2 = getOMLDataSet(did = 61L)  # the iris data set has the data set ID 61
iris.data2
```

```
## 
## Data Set "iris" :: (Version = 1, OpenML ID = 61)
##   Collection Date         : 1936
##   Creator(s)              : R.A. Fisher
##   Default Target Attribute: class
```

### Download an OpenML flow

**FIXME: `implementation.id` should be called `flow.id`**

You can download a flow by specifying the `implementation.id` parameter in the `getOMLFlow` function:


```r
flow = getOMLFlow(implementation.id = 1248L)
flow
```

```
## 
## Flow "classif.randomForest" :: (Version = 1, Implementation ID = 1248)
## 	External Version         : 4.6-10
## 	Dependencies             : mlr_2.3, randomForest_4.6.10
## 	Number of Flow Parameters: 12
## 	Number of Flow Components: 0
```

### Download an OpenML run
To download the results of one run including all server and user computed metrics, you have to know 
the corresponding run ID. These IDs can be extracted from the `runs` object, which was created in the
previous section. Here we use the first run of task 59, which has the `run.id` 234.
You can download a single OpenML run with the `getOMLRun` function:


```r
run = getOMLRun(run.id = 234L)  # see ?OMLRun for each slot of the OMLRun object
```

There are some slots which are of major intereset for the `OMLRun` object. A list containing the parameter settings
can be obtained by `run$parameter.setting`. All data that served as input for the run, including the
URL to the data is stored in `run$input.data`.

To retrieve predictions of an uploaded run, you can set the parameter `get.predictions = TRUE` to store the
predictions in the `$predictions` slot or use the function `getOMLPredictions(run)`:


```r
run.pred = getOMLRun(run.id = 234L, get.predictions = TRUE)
all.equal(run.pred$predictions, getOMLPredictions(run))
```

```
## [1] TRUE
```

----------------------------------------------------------------------------------------------------
Jump to:
[Introduction](1-Introduction.md)
[Configuration](2-Configuration.md)
[Stage 0 - Listing](3-Stage-0-Listing.md)
Stage 1 - Downloading
[Stage 2 - Running models on tasks](5-Stage-2-Running.md)
[Stage 3 - Uploading](6-Stage-3-Uploading.md)
[8 Example workflow with mlr](8-Example-workflow-with-mlr.md)
