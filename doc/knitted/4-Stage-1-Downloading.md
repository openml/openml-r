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
task
```

```
## 
## OpenML Task 1 :: (Data ID = 1)
## 	Task Type            : Supervised Classification
## 	Data Set             : anneal :: (Version = 2, OpenML ID = 1)
## 	Target Feature(s)    : class
## 	Estimation Procedure : Stratified crossvalidation (1 x 10 folds)
## 	Evaluation Measure(s): predictive accuracy
```
The corresponding data set can be accessed by

```r
str(task$data.set$data)
```

```
## 'data.frame':	898 obs. of  39 variables:
##  $ family                      : Factor w/ 10 levels "?","GB","GK",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ product.type                : Factor w/ 3 levels "C","H","G": 1 1 1 1 1 1 1 1 1 1 ...
##  $ steel                       : Factor w/ 9 levels "?","R","A","U",..: 3 2 2 3 3 3 2 3 2 3 ...
##  $ carbon                      : num  8 0 0 0 0 0 0 0 0 0 ...
##  $ hardness                    : num  0 0 0 60 60 45 0 0 0 0 ...
##  $ temper_rolling              : Factor w/ 2 levels "?","T": 1 1 1 2 2 1 1 1 1 1 ...
##  $ condition                   : Factor w/ 4 levels "?","S","A","X": 2 2 2 1 1 2 2 2 2 2 ...
##  $ formability                 : Factor w/ 6 levels "?","1","2","3",..: 1 3 3 1 1 1 3 3 3 4 ...
##  $ strength                    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ non.ageing                  : Factor w/ 2 levels "?","N": 1 1 1 1 1 1 1 1 1 2 ...
##  $ surface.finish              : Factor w/ 3 levels "?","P","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ surface.quality             : Factor w/ 5 levels "?","D","E","F",..: 5 3 3 5 5 2 3 3 3 3 ...
##  $ enamelability               : Factor w/ 6 levels "?","1","2","3",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ bc                          : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ bf                          : Factor w/ 2 levels "?","Y": 1 1 2 1 1 1 1 1 1 1 ...
##  $ bt                          : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ bw.2Fme                     : Factor w/ 3 levels "?","B","M": 1 1 2 3 2 1 1 1 1 1 ...
##  $ bl                          : Factor w/ 2 levels "?","Y": 1 1 1 1 2 1 2 1 1 1 ...
##  $ m                           : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ chrom                       : Factor w/ 2 levels "?","C": 1 1 1 1 1 1 1 1 1 1 ...
##  $ phos                        : Factor w/ 2 levels "?","P": 1 1 1 1 1 1 1 1 1 1 ...
##  $ cbond                       : Factor w/ 2 levels "?","Y": 1 1 1 1 2 1 1 1 1 1 ...
##  $ marvi                       : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ exptl                       : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ ferro                       : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ corr                        : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ blue.2Fbright.2Fvarn.2Fclean: Factor w/ 5 levels "?","B","R","V",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ lustre                      : Factor w/ 2 levels "?","Y": 1 2 1 1 1 1 1 2 2 1 ...
##  $ jurofm                      : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ s                           : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ p                           : Factor w/ 2 levels "?","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ shape                       : Factor w/ 2 levels "COIL","SHEET": 1 1 2 1 2 1 2 1 1 2 ...
##  $ thick                       : num  0.7 3.2 0.7 2.801 0.801 ...
##  $ width                       : num  610 610 1300 385 255 ...
##  $ len                         : num  0 0 762 0 269 0 4880 0 0 762 ...
##  $ oil                         : Factor w/ 3 levels "?","Y","N": 1 1 1 1 1 1 2 1 1 1 ...
##  $ bore                        : Factor w/ 4 levels "0","500","600",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ packing                     : Factor w/ 4 levels "?","1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
##  $ class                       : Factor w/ 6 levels "1","2","3","4",..: 3 3 3 3 3 3 3 3 3 3 ...
```

### Download an OpenML data set only
OpenML tasks have predefined estimation procedures and measures. Sometimes you might want to deviate
from these fixings. Of course, it is possible to define new tasks that match your desires, but this
will not always be the means of choice -- e.g., when you want to run a few preliminary experiments.
For this matter, you can use the function `getOMLDataSet`, which accepts not only tasks (as seen in the section above) but also a data set ID as input:


```r
anneal.data = getOMLDataSet(x = 1L)  # the anneal data set has the data set ID (did = 1, see also previous section)
anneal.data
```

```
## 
## Data Set "anneal" :: (Version = 2, OpenML ID = 1)
## 	Default Target Attribute: class
```

```r
anneal.data = getOMLDataSet(x = task)  # the task defined above (with task.id = 1) uses the anneal data
anneal.data
```

```
## 
## Data Set "anneal" :: (Version = 2, OpenML ID = 1)
## 	Default Target Attribute: class
```

### Download an OpenML run
To download the results of one run including all server and user computed metrics, you have to know the corresponding run ID. You can download a single OpenML run with the `getOMLRun` function:


```r
run = getOMLRun(run.id = 1L)  # see ?OMLRun for each slot of the OMLRun object
```

Some important slots for the `OMLRun` object are:


```r
run$parameter.setting  # A list containing information on the parameter settings.
```

```
## [[1]]
##  I = 0.0
## 
## [[2]]
##  L = -1
## 
## [[3]]
##  M = 2
## 
## [[4]]
##  N = 3
## 
## [[5]]
##  S = 1
## 
## [[6]]
##  V = 0.001
```

```r
run$input.data  # All data that served as input for the run, including the URL to the data.
```

```
## 
## ** Data Sets **
##   did        name
## 1   2 anneal.ORIG
## 2   9       autos
## 3  54     vehicle
##                                                                 url
## 1 http://openml.liacs.nl/data/download/2/dataset_2_anneal.ORIG.arff
## 2       http://openml.liacs.nl/data/download/9/dataset_9_autos.arff
## 3   http://openml.liacs.nl/data/download/54/dataset_54_vehicle.arff
## 
## ** Files **
## data frame with 0 columns and 0 rows
## 
## ** Evaluations **
## data frame with 0 columns and 0 rows
```

```r
run$output.data$evaluations
```

```
##    did                          name implementation label       value
## 1   NA          area_under_roc_curve              4  <NA>    0.839359
## 2   NA              confusion_matrix             10  <NA>          NA
## 3   NA                     f_measure             12  <NA>    0.600026
## 4   NA                         kappa             13  <NA>    0.491678
## 5   NA kb_relative_information_score             14  <NA> 1063.298606
## 6   NA           mean_absolute_error             21  <NA>    0.127077
## 7   NA     mean_prior_absolute_error             27  <NA>    0.220919
## 8   NA           number_of_instances             34  <NA> 2050.000000
## 9   NA                os_information             53  <NA>          NA
## 10  NA                     precision             35  <NA>    0.599589
## 11  NA           predictive_accuracy             36  <NA>    0.614634
## 12  NA                 prior_entropy             38  <NA>    2.326811
## 13  NA                        recall             39  <NA>    0.614634
## 14  NA       relative_absolute_error             40  <NA>    0.575218
## 15  NA root_mean_prior_squared_error             41  <NA>    0.331758
## 16  NA       root_mean_squared_error             42  <NA>    0.280656
## 17  NA   root_relative_squared_error             43  <NA>    0.845964
## 18  NA             scimark_benchmark             55  <NA> 1973.409151
##    stdev
## 1     NA
## 2     NA
## 3     NA
## 4     NA
## 5     NA
## 6     NA
## 7     NA
## 8     NA
## 9     NA
## 10    NA
## 11    NA
## 12    NA
## 13    NA
## 14    NA
## 15    NA
## 16    NA
## 17    NA
## 18    NA
##                                                                                                                                       array.data
## 1                                                                                     [�,0.99113,0.898048,0.874862,0.791282,0.807343,0.820674]
## 2  [[0,0,0,0,0,0,0],[0,0,30,0,0,0,0],[0,0,173,12,29,3,3],[0,0,24,515,87,41,3],[0,0,10,115,353,28,34],[0,0,14,77,77,127,25],[0,0,15,11,88,64,92]]
## 3                                                                                             [0,0,0.711934,0.735714,0.601363,0.435678,0.430913]
## 4                                                                                                                                           <NA>
## 5                                                                                                                                           <NA>
## 6                                                                                                                                           <NA>
## 7                                                                                                                                           <NA>
## 8                                                                                                                     [0,30,220,670,540,320,270]
## 9                                                                            [ Oracle Corporation, 1.7.0_51, amd64, Linux, 3.7.10-1.28-desktop ]
## 10                                                                                             [0,0,0.650376,0.705479,0.556782,0.48289,0.585987]
## 11                                                                                                                                          <NA>
## 12                                                                                                                                          <NA>
## 13                                                                                            [0,0,0.786364,0.768657,0.653704,0.396875,0.340741]
## 14                                                                                                                                          <NA>
## 15                                                                                                                                          <NA>
## 16                                                                                                                                          <NA>
## 17                                                                                                                                          <NA>
## 18                                          [ 1262.1133708514062, 1630.9393838458018, 932.0675956790141, 1719.5408190761134, 4322.384586656718 ]
##    sample.size
## 1           NA
## 2           NA
## 3           NA
## 4           NA
## 5           NA
## 6           NA
## 7           NA
## 8           NA
## 9           NA
## 10          NA
## 11          NA
## 12          NA
## 13          NA
## 14          NA
## 15          NA
## 16          NA
## 17          NA
## 18          NA
```

To retrieve predictions of an uploaded run, you can set the parameter `get.predictions = TRUE` to store the
predictions in the `$predictions` slot or use the function `getOMLPredictions(run)`:


```r
run.pred = getOMLRun(run.id = 1L, get.predictions = TRUE)
all.equal(run.pred$predictions, getOMLPredictions(run))
```

```
## [1] TRUE
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
