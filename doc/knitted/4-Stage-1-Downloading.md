Stage 1 - Downloading
=====================

### Download an OpenML task
Each OpenML task is a bundle of a data set, a target feature, an estimation procedure (e.g.,
10-fold CV), data splits fitting this estimation procedure and, finally, one or more evaluation measures. Every task has a type, e.g., Supervised Classification or Supervised Regression. A user can download such a task from the OpenML server, compute predictions with an algorithm (called "flow" or "implementation") and upload this algorithm as well as the predictions. The server will then calculate many different measures and add them to the data base.

To download a certain task from the OpenML server, you need to know the task's ID. See
[section 3](3-Stage-0-Listing.md) to learn how to retrieve a list of all available tasks in R.

The following call returns an OpenML task object: 


```splus
task = getOMLTask(id = 4L, session.hash)
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
Note, that the corresponding data set is not yet retrieved:

```splus
task$data.set$data
```

```
## NULL
```
After running the following line, the task is complete:

```splus
task$data.set = getOMLDataSet(task)
summary(task$data.set$data)
```

```
##     duration    wage.increase.first.year wage.increase.second.year
##  Min.   :1.00   Min.   :2.0              Min.   :2.00             
##  1st Qu.:2.00   1st Qu.:2.5              1st Qu.:3.00             
##  Median :2.00   Median :4.0              Median :4.00             
##  Mean   :2.16   Mean   :3.8              Mean   :3.97             
##  3rd Qu.:3.00   3rd Qu.:4.5              3rd Qu.:4.50             
##  Max.   :3.00   Max.   :7.0              Max.   :7.00             
##  NA's   :1      NA's   :1                NA's   :11               
##  wage.increase.third.year cost.of.living.adjustment working.hours
##  Min.   :2.00             none:22                   Min.   :27   
##  1st Qu.:2.40             tcf : 8                   1st Qu.:37   
##  Median :4.60             tc  : 7                   Median :38   
##  Mean   :3.91             NA's:20                   Mean   :38   
##  3rd Qu.:5.00                                       3rd Qu.:40   
##  Max.   :5.10                                       Max.   :40   
##  NA's   :42                                         NA's   :6    
##        pension    standby.pay    shift.differential education.allowance
##  none      :11   Min.   : 2.00   Min.   : 0.00      yes :10            
##  ret_allw  : 4   1st Qu.: 2.00   1st Qu.: 3.00      no  :12            
##  empl_contr:12   Median : 8.00   Median : 4.00      NA's:35            
##  NA's      :30   Mean   : 7.44   Mean   : 4.87                         
##                  3rd Qu.:12.00   3rd Qu.: 5.00                         
##                  Max.   :14.00   Max.   :25.00                         
##                  NA's   :48      NA's   :26                            
##  statutory.holidays          vacation  longterm.disability.assistance
##  Min.   : 9.0       below_average:18   yes :20                       
##  1st Qu.:10.0       average      :17   no  : 8                       
##  Median :11.0       generous     :16   NA's:29                       
##  Mean   :11.1       NA's         : 6                                 
##  3rd Qu.:12.0                                                        
##  Max.   :15.0                                                        
##  NA's   :4                                                           
##  contribution.to.dental.plan bereavement.assistance
##  none: 9                     yes :27               
##  half:15                     no  : 3               
##  full:13                     NA's:27               
##  NA's:20                                           
##                                                    
##                                                    
##                                                    
##  contribution.to.health.plan  class   
##  none: 8                     bad :20  
##  half: 9                     good:37  
##  full:20                              
##  NA's:20                              
##                                       
##                                       
## 
```

### Download an OpenML data set only
OpenML tasks have predefined estimation procedures and measures. Sometimes you might want to deviate
from these fixings. Of course, it is possible to define new tasks that match your desires, but this
will not always be the means of choice -- e.g., when you want to run a few preliminary experiments.
For this matter, you can use the function `getOMLDataSet`, which accepts not only tasks (as seen in the section above) but also a data set ID as input:


```splus
oml.data = getOMLDataSet(1, session.hash)
oml.data
```

```
## 
## Data Set "anneal" :: (Version = 2, OpenML ID = 1)
## 	Default Target Attribute: class
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
